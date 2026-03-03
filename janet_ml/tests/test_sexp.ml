(** Tests for sexp_of_t across all modules that expose it. *)

open! Core
open Janet_ml
open Janet_ml.Janet

(** Helper: try sexp_of_t and print either the sexp or the exception. *)
let try_sexp f =
  match f () with
  | sexp -> print_s sexp
  | exception exn -> printf "RAISED: %s\n" (Exn.to_string exn)
;;

(* -- Safe: Janet.sexp_of_t on primitive values -- *)

let%expect_test "Janet.sexp_of_t on nil" =
  with_janet (fun () -> try_sexp (fun () -> sexp_of_t (create ())));
  [%expect {| Nil |}]
;;

let%expect_test "Janet.sexp_of_t on number" =
  with_janet (fun () -> try_sexp (fun () -> sexp_of_t (of_int 42)));
  [%expect {| (Number 42) |}]
;;

let%expect_test "Janet.sexp_of_t on bool" =
  with_janet (fun () ->
    try_sexp (fun () -> sexp_of_t (Unwrapped.to_janet (Boolean true))));
  [%expect {| (Boolean true) |}]
;;

let%expect_test "Janet.sexp_of_t on string" =
  with_janet (fun () -> try_sexp (fun () -> sexp_of_t (of_string "hello")));
  [%expect {| (String hello) |}]
;;

let%expect_test "Janet.sexp_of_t on keyword" =
  with_janet (fun () -> try_sexp (fun () -> sexp_of_t (of_keyword "foo")));
  [%expect {| (Keyword foo) |}]
;;

let%expect_test "Janet.sexp_of_t on symbol" =
  with_janet (fun () -> try_sexp (fun () -> sexp_of_t (of_symbol "bar")));
  [%expect {| (Symbol bar) |}]
;;

(* -- Safe: container sexp_of_t -- *)

let%expect_test "Array.sexp_of_t" =
  with_janet (fun () ->
    let arr = Array.of_list [ of_int 1; of_int 2; of_int 3 ] in
    try_sexp (fun () -> Array.sexp_of_t arr));
  [%expect {| (Array ((Number 1) (Number 2) (Number 3))) |}]
;;

let%expect_test "Buffer.sexp_of_t" =
  with_janet (fun () ->
    let buf = Buffer.create 16 in
    Buffer.push_string buf "hello";
    try_sexp (fun () -> Buffer.sexp_of_t buf));
  [%expect {| (Buffer hello) |}]
;;

let%expect_test "Tuple.sexp_of_t" =
  with_janet (fun () ->
    let tup = Tuple.of_list [ of_int 1; of_int 2; of_int 3 ] in
    try_sexp (fun () -> Tuple.sexp_of_t tup));
  [%expect {| (Tuple ((Number 1) (Number 2) (Number 3))) |}]
;;

let%expect_test "Table.sexp_of_t" =
  with_janet (fun () ->
    let tbl = Table.of_pairs [ of_keyword "a", of_int 1 ] in
    try_sexp (fun () -> Table.sexp_of_t tbl));
  [%expect {| (Table (((Keyword a) (Number 1)))) |}]
;;

let%expect_test "Struct.sexp_of_t" =
  with_janet (fun () ->
    let st = Struct.of_pairs [ of_keyword "a", of_int 1 ] in
    try_sexp (fun () -> Struct.sexp_of_t st));
  [%expect {| (Struct (((Keyword a) (Number 1)))) |}]
;;

(* -- Safe: Kv.sexp_of_t -- *)

let%expect_test "Kv.sexp_of_t" =
  with_janet (fun () ->
    let st = Struct.of_pairs [ of_keyword "x", of_int 42 ] in
    let kv = Struct.find st (of_keyword "x") in
    try_sexp (fun () -> Kv.sexp_of_t kv));
  [%expect {| ((Keyword x) (Number 42)) |}]
;;

(* -- Safe: Fiber.sexp_of_t -- *)

let%expect_test "Fiber.sexp_of_t" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn noop [] nil)" ~source_path:None in
    let fn =
      match
        Env.Binding.lookup ~env "noop" |> Env.Binding.to_janet |> Unwrapped.of_janet
      with
      | Unwrapped.Function f -> f
      | _ -> failwith "not a function"
    in
    let fiber = Fiber.create fn ~capacity:64 ~argv:[] in
    try_sexp (fun () -> Fiber.sexp_of_t fiber));
  [%expect {| New |}]
;;

(* -- Safe: Unwrapped.sexp_of_t on Int/UInt -- *)

let%expect_test "Unwrapped.sexp_of_t on Int variant" =
  with_janet (fun () -> try_sexp (fun () -> Unwrapped.sexp_of_t (Unwrapped.Int 42L)));
  [%expect {| (Int 42) |}]
;;

let%expect_test "Unwrapped.sexp_of_t on UInt variant" =
  with_janet (fun () ->
    try_sexp (fun () -> Unwrapped.sexp_of_t (Unwrapped.UInt (Unsigned.UInt64.of_int 99))));
  [%expect {| (UInt 99) |}]
;;

(* -- Potentially broken: Function.sexp_of_t -- *)

let%expect_test "Function.sexp_of_t" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn double [x] (* x 2))" ~source_path:None in
    let fn =
      match
        Env.Binding.lookup ~env "double" |> Env.Binding.to_janet |> Unwrapped.of_janet
      with
      | Unwrapped.Function f -> f
      | _ -> failwith "not a function"
    in
    try_sexp (fun () -> Function.sexp_of_t fn));
  [%expect {| "<function double>" |}]
;;

(* -- Potentially broken: Unwrapped.sexp_of_t on Function variant -- *)

let%expect_test "Unwrapped.sexp_of_t on Function variant" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn double [x] (* x 2))" ~source_path:None in
    let fn =
      match
        Env.Binding.lookup ~env "double" |> Env.Binding.to_janet |> Unwrapped.of_janet
      with
      | Unwrapped.Function f -> f
      | _ -> failwith "not a function"
    in
    try_sexp (fun () -> Unwrapped.sexp_of_t (Unwrapped.Function fn)));
  [%expect {| (Function "<function double>") |}]
;;

(* -- Potentially broken: Cfunction.sexp_of_t -- *)

let%expect_test "Cfunction.sexp_of_t" =
  with_janet_env (fun env ->
    let unwrap_num j = Janet_c.C.Functions.janet_unwrap_number j in
    let wrap_num n = Janet_c.C.Functions.janet_wrap_number n in
    let argv_get argv i =
      Ctypes.CArray.from_ptr argv (i + 1) |> fun a -> Ctypes.CArray.get a i
    in
    let _cb =
      Cfun.register_raw ~env "ocaml-id" (fun _argc argv ->
        wrap_num (argv_get argv 0 |> unwrap_num))
    in
    let cfn =
      match
        Env.Binding.lookup ~env "ocaml-id" |> Env.Binding.to_janet |> Unwrapped.of_janet
      with
      | Unwrapped.CFunction f -> f
      | _ -> failwith "not a cfunction"
    in
    let sexp = Cfunction.sexp_of_t cfn in
    match sexp with
    | Sexp.Atom s when String.is_prefix s ~prefix:"<cfunction " ->
      printf "OK: cfunction atom\n"
    | _ -> printf "UNEXPECTED: %s\n" (Sexp.to_string sexp));
  [%expect {| OK: cfunction atom |}]
;;

(* -- Potentially broken: Janet.sexp_of_t on a function value -- *)

let%expect_test "Janet.sexp_of_t on a function value" =
  with_janet_env (fun env ->
    let fn_val =
      Janet_ml.dostring_exn ~env "(defn triple [x] (* x 3))" ~source_path:None
    in
    try_sexp (fun () -> sexp_of_t fn_val));
  [%expect {| (Function "<function triple>") |}]
;;

(* -- Abstract.sexp_of_t (harmless placeholder) -- *)

let%expect_test "Abstract.sexp_of_t" =
  with_janet (fun () ->
    try_sexp (fun () -> Abstract.sexp_of_t (Ctypes.from_voidp Ctypes.void Ctypes.null)));
  [%expect {| janet_abstract |}]
;;
