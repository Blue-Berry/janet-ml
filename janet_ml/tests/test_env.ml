(** Tests for Env.def, Env.var, and Binding.lookup. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "Env.def makes a value visible to Janet code" =
  with_janet_env (fun env ->
    Env.def env "my-pi" (Unwrapped.to_janet (Unwrapped.Number 3.14159));
    Janet_ml.dostring_exn ~env "(* my-pi 2)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 6.28318) |}]
;;

let%expect_test "Env.def with a string value" =
  with_janet_env (fun env ->
    Env.def env "greeting" (Unwrapped.to_janet (Unwrapped.String "hello"));
    Janet_ml.dostring_exn ~env {|(string greeting " world")|} ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (String "hello world") |}]
;;

let%expect_test "Env.def with a keyword value" =
  with_janet_env (fun env ->
    Env.def env "my-tag" (Unwrapped.to_janet (Unwrapped.Keyword "active"));
    Janet_ml.dostring_exn ~env "(= my-tag :active)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Boolean true) |}]
;;

let%expect_test "Env.def with an array value" =
  with_janet_env (fun env ->
    let arr =
      Array.of_list
        Unwrapped.[ to_janet (Number 1.0); to_janet (Number 2.0); to_janet (Number 3.0) ]
    in
    Env.def env "my-array" (Array.wrap arr);
    Janet_ml.dostring_exn ~env "(length my-array)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 3) |}]
;;

let%expect_test "multiple Env.def bindings are all visible" =
  with_janet_env (fun env ->
    Env.def env "a" (Unwrapped.to_janet (Unwrapped.Number 10.0));
    Env.def env "b" (Unwrapped.to_janet (Unwrapped.Number 20.0));
    Env.def env "c" (Unwrapped.to_janet (Unwrapped.Number 30.0));
    Janet_ml.dostring_exn ~env "(+ a b c)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 60) |}]
;;

let%expect_test "Env.var creates a mutable binding accessible from Janet" =
  with_janet_env (fun env ->
    (* janet_var creates a var binding; Janet sees it as a mutable box.
       Use Janet's own (var) + (set) for mutation. *)
    let _ = Janet_ml.dostring_exn ~env "(var counter 0)" ~source_path:None in
    let _ = Janet_ml.dostring_exn ~env "(set counter 42)" ~source_path:None in
    Janet_ml.dostring_exn ~env "counter" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 42) |}]
;;

let%expect_test "Binding.lookup returns None for an unknown name" =
  with_janet_env (fun env ->
    match Env.Binding.lookup ~env "totally-unknown-symbol-xyz" with
    | Env.Binding.None -> print_endline "None"
    | _ -> print_endline "found");
  [%expect {| None |}]
;;

let%expect_test "Binding.lookup returns Def after Env.def" =
  with_janet_env (fun env ->
    Env.def env "defined-val" (Unwrapped.to_janet (Unwrapped.Number 99.0));
    match Env.Binding.lookup ~env "defined-val" with
    | Env.Binding.Def v -> v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s
    | _ -> print_endline "wrong binding kind");
  [%expect {| (Number 99) |}]
;;

let%expect_test "Binding.lookup returns Var kind after Env.var" =
  with_janet_env (fun env ->
    Env.var env "mutable-val" (Unwrapped.to_janet (Unwrapped.Number 7.0));
    (* janet_var stores the value in a single-element array (the mutable box).
       Binding.lookup returns Var with that box as the Janet value. *)
    match Env.Binding.lookup ~env "mutable-val" with
    | Env.Binding.Var _ -> print_endline "got Var binding"
    | _ -> print_endline "wrong binding kind");
  [%expect {| got Var binding |}]
;;

let%expect_test "Binding.lookup finds a Janet defn as a Function" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn double [x] (* x 2))" ~source_path:None in
    match Env.Binding.lookup ~env "double" with
    | Env.Binding.Def v ->
      (match Unwrapped.of_janet v with
       | Unwrapped.Function _ -> print_endline "is a function"
       | _ -> print_endline "not a function")
    | _ -> print_endline "wrong binding kind");
  [%expect {| is a function |}]
;;

let%expect_test "Binding.lookup finds built-in Janet functions" =
  with_janet_env (fun env ->
    match Env.Binding.lookup ~env "+" with
    | Env.Binding.Def v ->
      (match Unwrapped.of_janet v with
       | Unwrapped.CFunction _ -> print_endline "cfunction"
       | Unwrapped.Function _ -> print_endline "function"
       | _ -> print_endline "other")
    | _ -> print_endline "not found");
  [%expect {| function |}]
;;

let%expect_test "Extend env" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn double [x] (* x 2))" ~source_path:None in
    match Env.Binding.lookup ~env "double" with
    | Env.Binding.Def v ->
      let extras = Table.create 4 in
      Table.put extras ~key:(Janet.of_symbol "double") ~value:v;
      let env = Env.core_env ~replacements:extras () in
      let res = Janet_ml.dostring_exn ~env "(double 21)" ~source_path:None in
      pretty res |> print_endline
    | _ -> failwith "wrong binding kind");
  [%expect {| 42 |}]
;;

(* Just use the prev env as replacements *)
let%expect_test "Extend env" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn double [x] (* x 2))" ~source_path:None in
    let env = Env.core_env ~replacements:env () in
    let res = Janet_ml.dostring_exn ~env "(double 21)" ~source_path:None in
    pretty res |> print_endline);
  [%expect {| 42 |}]
;;
