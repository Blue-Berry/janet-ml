open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "Test compile janet" =
  init ();
  let src =
    {|
(defn test [a b] (+ a b))

(+ 1 2)
(+ 2 2)
(test 3 2)
|}
  in
  let open Janet in
  let env = Env.core_env () in
  Janet_ml.dostring_exn ~env src ~source_path:"test"
  |> Unwrapped.of_janet
  |> Unwrapped.sexp_of_t
  |> print_s;
  [%expect {| (Number 5) |}]
;;

(* janet_resolve(env, janet_csymbol("load-image-dict"), &lidv); *)

let%expect_test "Test resolve and call janet function" =
  init ();
  let src =
    {|
(defn test [a b] (+ a b))
(defn main [&] (test 1 2))
|}
  in
  let open Janet in
  let env = Env.core_env () in
  let _out = Janet_ml.dostring_exn ~env src ~source_path:"test" in
  let main = Env.Binding.lookup ~env "main" |> Env.Binding.to_janet in
  (match Unwrapped.of_janet main with
   | Unwrapped.Function main ->
     let f = Fiber.create main ~capacity:64 ~argv:[] in
     Fiber.continue f (Janet.create ())
     |> snd
     |> Unwrapped.of_janet
     |> Unwrapped.sexp_of_t
     |> Sexp.to_string_hum
   | _ -> "Not a function")
  |> print_endline;
  [%expect {| (Number 3) |}]
;;

let%expect_test "Test resolve and call janet function" =
  init ();
  let src =
    {|
(defn test [a b] (+ a b))
(defn main [&] (test 1 2))
|}
  in
  let open Janet in
  let env = Env.core_env () in
  let _out = Janet_ml.dostring_exn ~env src ~source_path:"test" in
  let image = Marshal.marshal_symbol ~env "main" in
  let main = Marshal.unmarshal image in
  (match Unwrapped.of_janet main with
   | Unwrapped.Function main ->
     let f = Fiber.create main ~capacity:64 ~argv:[] in
     Fiber.continue f (Janet.create ())
     |> snd
     |> Unwrapped.of_janet
     |> Unwrapped.sexp_of_t
     |> Sexp.to_string_hum
   | _ -> "Not a function")
  |> print_endline;
  [%expect {| (Number 3) |}]
;;

let%expect_test "dostring error handling" =
  init ();
  let env = Env.core_env () in
  let signal, _value = Janet_ml.dostring ~env "(error \"fail\")" ~source_path:"test" in
  (match signal with
   | Fiber.Signal_ok -> print_endline "ok"
   | Fiber.Signal_error -> print_endline "error"
   | _ -> print_endline "other");
  [%expect
    {|
    error: fail
      in thunk [test] on line 1, column 1
    error
    |}]
;;

let%expect_test "dostring_exn raises Janet_error on failure" =
  init ();
  let env = Env.core_env () in
  (try
     let _ = Janet_ml.dostring_exn ~env "(error \"boom\")" ~source_path:"test" in
     print_endline "no exception"
   with
   | Janet_ml.Janet_error msg ->
     (* just print whether we got the exception, not the exact message *)
     ignore msg;
     print_endline "caught Janet_error");
  [%expect
    {|
    error: boom
      in thunk [test] on line 1, column 1
    caught Janet_error
    |}]
;;

let%expect_test "Janet.to_string and Janet.pretty" =
  init ();
  let env = Env.core_env () in
  let v = Janet_ml.dostring_exn ~env {|{:a 1 :b 2}|} ~source_path:"test" in
  (* to_string on a struct gives a pointer description like "<struct 0x...>" *)
  let s = to_string v in
  print_endline (if String.is_prefix s ~prefix:"<struct" then "struct-like" else s);
  [%expect {| struct-like |}]
;;

let%expect_test "Table.of_pairs and Table.iter" =
  init ();
  let k1 = Unwrapped.to_janet (Unwrapped.Keyword "x") in
  let k2 = Unwrapped.to_janet (Unwrapped.Keyword "y") in
  let v1 = Unwrapped.to_janet (Unwrapped.Number 10.0) in
  let v2 = Unwrapped.to_janet (Unwrapped.Number 20.0) in
  let tbl = Table.of_pairs [ k1, v1; k2, v2 ] in
  let sum =
    Table.fold tbl ~init:0.0 ~f:(fun acc _k v ->
      match Unwrapped.of_janet v with
      | Unwrapped.Number n -> acc +. n
      | _ -> acc)
  in
  Printf.printf "sum = %.0f\n" sum;
  [%expect {| sum = 30 |}]
;;

let%expect_test "Env.def and Binding.lookup" =
  init ();
  let env = Env.core_env () in
  let val_ = Unwrapped.to_janet (Unwrapped.Number 42.0) in
  Env.def env "my-const" val_;
  let looked_up = Env.Binding.lookup ~env "my-const" |> Env.Binding.to_janet in
  looked_up |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s;
  [%expect {| (Number 42) |}]
;;

let%expect_test "with_root keeps value alive" =
  init ();
  let env = Env.core_env () in
  let v = Janet_ml.dostring_exn ~env {|(+ 5 5)|} ~source_path:"test" in
  (* root v across another allocation *)
  Janet.with_root v ~f:(fun rooted ->
    let _other = Janet_ml.dostring_exn ~env {|(+ 1 1)|} ~source_path:"test" in
    rooted |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s);
  [%expect {| (Number 10) |}]
;;

let%expect_test "register_cfun - OCaml callback from Janet" =
  init ();
  let env = Env.core_env () in
  let _cb =
    Cfun.register_raw ~env "ocaml-square" (fun argc argv ->
      assert (Int32.equal argc 1l);
      let args = Ctypes.CArray.from_ptr argv 1 in
      let x = Ctypes.CArray.get args 0 |> Janet_c.C.Functions.janet_unwrap_number in
      Janet_c.C.Functions.janet_wrap_number (x *. x))
  in
  Janet_ml.dostring_exn ~env {|(ocaml-square 7)|} ~source_path:"test"
  |> Unwrapped.of_janet
  |> Unwrapped.sexp_of_t
  |> print_s;
  [%expect {| (Number 49) |}]
;;

let%expect_test "Fiber.cancel" =
  init ();
  let env = Env.core_env () in
  let _out =
    Janet_ml.dostring_exn
      ~env
      {|(defn looper [] (forever (yield 1)))|}
      ~source_path:"test"
  in
  let fn_ =
    match
      Env.Binding.lookup ~env "looper" |> Env.Binding.to_janet |> Unwrapped.of_janet
    with
    | Unwrapped.Function f -> f
    | _ -> failwith "not a function"
  in
  let fiber = Fiber.create fn_ ~capacity:64 ~argv:[] in
  let _sig, _v = Fiber.continue fiber (create ()) in
  (* cancel the fiber with an error message *)
  let err_msg = Unwrapped.to_janet (Unwrapped.String "cancelled") in
  Fiber.cancel fiber err_msg;
  let status = Fiber.status fiber in
  (match status with
   | Fiber.Error -> print_endline "fiber errored after cancel"
   | _ -> print_endline "unexpected status");
  [%expect {| fiber errored after cancel |}]
;;

let%expect_test "Unmarshal jimage" =
  init ();
  let image = Embedded_files.janet_image_dot_jimage in
  let env =
    Marshal.unmarshal image
    |> Unwrapped.of_janet
    |> function
    | Unwrapped.Table t -> t
    | _ -> failwith "Not a table"
  in
  let double =
    Env.Binding.lookup ~env "double"
    |> Env.Binding.to_janet
    |> Unwrapped.of_janet
    |> function
    | Unwrapped.Function t -> t
    | _ -> failwith "Not a function"
  in
  let arg = Unwrapped.to_janet (Unwrapped.Number 21.0) in
  Function.call_exn double [ arg ] |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s;
  [%expect {| (Number 42) |}]
;;

let%expect_test "Unmarshal jimage 2" =
  init ();
  let image = Embedded_files.janet_image_dot_jimage in
  let env =
    Marshal.unmarshal image
    |> Unwrapped.of_janet
    |> function
    | Unwrapped.Table t -> t
    | _ -> failwith "Not a table"
  in
  (* Table.to_janet env |> pretty |> print_endline; *)
  (* let env = Env.core_env ~replacements:env () in *)
  (* Table.to_janet env |> pretty |> print_endline; *)
  dostring ~env "(hello)" |> ignore;
  [%expect {| "Hello World" |}]
;;
