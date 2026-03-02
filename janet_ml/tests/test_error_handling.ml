(** Tests for dostring/dobytes error handling and with_janet/with_janet_env. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "dostring returns Signal_ok for a successful evaluation" =
  with_janet_env (fun env ->
    let signal, value = Janet_ml.dostring ~env "(+ 1 2)" ~source_path:None in
    (match signal with
     | Janet_c.C.Types.Signal_ok -> print_string "ok "
     | _ -> print_string "unexpected ");
    value |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s);
  [%expect {| ok (Number 3) |}]
;;

let%expect_test "dostring returns Signal_error when Janet raises an error" =
  with_janet_env (fun env ->
    let signal, _value = Janet_ml.dostring ~env "(error \"boom\")" ~source_path:None in
    match signal with
    | Janet_c.C.Types.Signal_error -> print_endline "error"
    | Janet_c.C.Types.Signal_ok -> print_endline "ok"
    | _ -> print_endline "other");
  [%expect
    {|
    error: boom
      in thunk pc=1
    error
    |}]
;;

let%expect_test "dostring returns Signal_error on a compile error" =
  with_janet_env (fun env ->
    let signal, _ = Janet_ml.dostring ~env "(defn broken [" ~source_path:(Some "test") in
    match signal with
    | Janet_c.C.Types.Signal_error -> print_endline "error"
    | _ -> print_endline "unexpected");
  [%expect
    {|
    test:1:14: parse error: unexpected end of source, [ opened at line 1, column 14
    error
    |}]
;;

let%expect_test "dostring returns Signal_error on an explicit error call" =
  with_janet_env (fun env ->
    (* Janet (/ 1 0) returns inf, not an error. Use (error ...) to force a signal. *)
    let signal, _ = Janet_ml.dostring ~env {|(error "forced")|} ~source_path:None in
    match signal with
    | Janet_c.C.Types.Signal_error -> print_endline "error"
    | _ -> print_endline "unexpected");
  [%expect
    {|
    error: forced
      in thunk pc=1
    error
    |}]
;;

let%expect_test "dostring_exn returns the value on success" =
  with_janet_env (fun env ->
    Janet_ml.dostring_exn ~env "(* 6 7)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 42) |}]
;;

let%expect_test "dostring_exn raises Janet_error on failure" =
  with_janet_env (fun env ->
    try
      ignore
        (Janet_ml.dostring_exn ~env {|(error "something went wrong")|} ~source_path:None);
      print_endline "no exception raised"
    with
    | Janet_ml.Janet_error msg ->
      if String.is_substring msg ~substring:"error"
      then print_endline "got Janet_error"
      else Printf.printf "unexpected: %s\n" msg);
  [%expect
    {|
    error: something went wrong
      in thunk pc=1
    got Janet_error
    |}]
;;

let%expect_test "dostring_exn error message includes the Janet value" =
  with_janet_env (fun env ->
    try
      ignore (Janet_ml.dostring_exn ~env {|(error "my-error-token")|} ~source_path:None)
    with
    | Janet_ml.Janet_error msg ->
      if String.is_substring msg ~substring:"my-error-token"
      then print_endline "error message contains Janet value"
      else Printf.printf "message: %s\n" msg);
  [%expect
    {|
    error: my-error-token
      in thunk pc=1
    error message contains Janet value
    |}]
;;

let%expect_test "Function.call_exn returns the value on success" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn add [a b] (+ a b))" ~source_path:None in
    let fn_ =
      match
        Env.Binding.lookup ~env "add" |> Env.Binding.to_janet |> Unwrapped.of_janet
      with
      | Unwrapped.Function f -> f
      | _ -> failwith "not a function"
    in
    Function.call_exn
      fn_
      [ Unwrapped.to_janet (Unwrapped.Number 10.0)
      ; Unwrapped.to_janet (Unwrapped.Number 32.0)
      ]
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 42) |}]
;;

let%expect_test "Function.call_exn raises Janet_error when the function errors" =
  with_janet_env (fun env ->
    let _ =
      Janet_ml.dostring_exn
        ~env
        "(defn always-fail [] (error \"nope\"))"
        ~source_path:None
    in
    let fn_ =
      match
        Env.Binding.lookup ~env "always-fail"
        |> Env.Binding.to_janet
        |> Unwrapped.of_janet
      with
      | Unwrapped.Function f -> f
      | _ -> failwith "not a function"
    in
    try
      ignore (Function.call_exn fn_ []);
      print_endline "no exception"
    with
    | Janet_ml.Janet_error _ -> print_endline "caught Janet_error");
  [%expect {| caught Janet_error |}]
;;

let%expect_test "with_janet initialises and deinitialises the VM" =
  Janet_ml.with_janet (fun () ->
    let env = Env.core_env ~replacements:None in
    Janet_ml.dostring_exn ~env "(+ 3 4)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 7) |}]
;;

let%expect_test "with_janet_env passes a ready-to-use environment" =
  Janet_ml.with_janet_env (fun env ->
    Janet_ml.dostring_exn ~env {|(string "hello" " " "world")|} ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (String "hello world") |}]
;;

let%expect_test "with_janet deinitialises even when the body raises" =
  (try Janet_ml.with_janet (fun () -> raise (Failure "inner error")) with
   | Failure msg -> Printf.printf "propagated: %s\n" msg);
  Janet_ml.with_janet_env (fun env ->
    Janet_ml.dostring_exn ~env "(+ 1 1)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect
    {|
    propagated: inner error
    (Number 2)
    |}]
;;
