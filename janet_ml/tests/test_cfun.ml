(** Tests for OCaml→Janet dynamic callbacks via register_cfun. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let unwrap_num j = Janet_c.C.Functions.janet_unwrap_number j
let wrap_num n = Janet_c.C.Functions.janet_wrap_number n

let argv_get argv i =
  Ctypes.CArray.from_ptr argv (i + 1) |> fun a -> Ctypes.CArray.get a i
;;

let%expect_test "unary callback is callable from Janet" =
  with_janet_env (fun env ->
    let _cb =
      register_cfun ~env "ocaml-square" (fun _argc argv ->
        wrap_num (argv_get argv 0 |> unwrap_num |> fun x -> x *. x))
    in
    Janet_ml.dostring_exn ~env "(ocaml-square 9)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 81) |}]
;;

let%expect_test "binary callback receives both arguments" =
  with_janet_env (fun env ->
    let _cb =
      register_cfun ~env "ocaml-add" (fun argc argv ->
        assert (Int32.equal argc 2l);
        wrap_num (unwrap_num (argv_get argv 0) +. unwrap_num (argv_get argv 1)))
    in
    Janet_ml.dostring_exn ~env "(ocaml-add 17 25)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 42) |}]
;;

let%expect_test "callback can return a string" =
  with_janet_env (fun env ->
    let _cb =
      register_cfun ~env "ocaml-greet" (fun _argc argv ->
        let name = argv_get argv 0 |> Janet_c.C.Functions.janet_unwrap_string in
        match Janet_c.C.Functions.janet_cstring ("Hello, " ^ name ^ "!") with
        | Some ptr -> Janet_c.C.Functions.janet_wrap_string ptr
        | None -> Janet_c.C.Functions.janet_wrap_nil ())
    in
    Janet_ml.dostring_exn ~env {|(ocaml-greet "world")|} ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (String "Hello, world!") |}]
;;

let%expect_test "callback can return nil" =
  with_janet_env (fun env ->
    let _cb =
      register_cfun ~env "ocaml-nil" (fun _argc _argv ->
        Janet_c.C.Functions.janet_wrap_nil ())
    in
    Janet_ml.dostring_exn ~env "(ocaml-nil)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| Nil |}]
;;

let%expect_test "multiple distinct callbacks coexist in one environment" =
  with_janet_env (fun env ->
    let _cb1 =
      register_cfun ~env "double" (fun _argc argv ->
        wrap_num (argv_get argv 0 |> unwrap_num |> ( *. ) 2.0))
    in
    let _cb2 =
      register_cfun ~env "triple" (fun _argc argv ->
        wrap_num (argv_get argv 0 |> unwrap_num |> ( *. ) 3.0))
    in
    let _cb3 =
      register_cfun ~env "negate" (fun _argc argv ->
        wrap_num (argv_get argv 0 |> unwrap_num |> ( ~-. )))
    in
    Janet_ml.dostring_exn ~env "(+ (double 5) (triple 3) (negate 2))" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 17) |}]
;;

let%expect_test "a registered callback can be called multiple times" =
  with_janet_env (fun env ->
    let call_count = ref 0 in
    let _cb =
      register_cfun ~env "ocaml-count" (fun _argc argv ->
        incr call_count;
        wrap_num (argv_get argv 0 |> unwrap_num |> ( +. ) 1.0))
    in
    let _ = Janet_ml.dostring_exn ~env "(ocaml-count 0)" ~source_path:None in
    let _ = Janet_ml.dostring_exn ~env "(ocaml-count 1)" ~source_path:None in
    let _ = Janet_ml.dostring_exn ~env "(ocaml-count 2)" ~source_path:None in
    Printf.printf "called %d times\n" !call_count);
  [%expect {| called 3 times |}]
;;

let%expect_test "registered callback works with Janet's map" =
  with_janet_env (fun env ->
    let _cb =
      register_cfun ~env "inc" (fun _argc argv ->
        wrap_num (argv_get argv 0 |> unwrap_num |> ( +. ) 1.0))
    in
    Janet_ml.dostring_exn ~env "(map inc [1 2 3 4 5])" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Array ((Number 2) (Number 3) (Number 4) (Number 5) (Number 6))) |}]
;;

let%expect_test "registered callback works with Janet's reduce" =
  with_janet_env (fun env ->
    let _cb =
      register_cfun ~env "ocaml-mul" (fun argc argv ->
        assert (Int32.equal argc 2l);
        wrap_num (unwrap_num (argv_get argv 0) *. unwrap_num (argv_get argv 1)))
    in
    Janet_ml.dostring_exn ~env "(reduce ocaml-mul 1 [1 2 3 4 5])" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 120) |}]
;;

let%expect_test "callback accumulates OCaml state across Janet calls" =
  with_janet_env (fun env ->
    let total = ref 0.0 in
    let _cb =
      register_cfun ~env "ocaml-accumulate" (fun _argc argv ->
        total := !total +. (argv_get argv 0 |> unwrap_num);
        wrap_num !total)
    in
    let _ = Janet_ml.dostring_exn ~env "(ocaml-accumulate 10)" ~source_path:None in
    let _ = Janet_ml.dostring_exn ~env "(ocaml-accumulate 20)" ~source_path:None in
    Janet_ml.dostring_exn ~env "(ocaml-accumulate 12)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 42) |}]
;;
