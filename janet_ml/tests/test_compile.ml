(** Tests for the Compile module. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "compile simple expression + run via to_function" =
  with_janet_env (fun env ->
    let p = Parser.create () in
    Parser.consume_string p "(+ 1 2)";
    let ast = Parser.produce p in
    match Compile.compile ast env "test" with
    | Ok fd ->
      let f = Compile.to_function fd in
      let result = Function.call_exn f [] in
      result |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s
    | Error e -> Printf.printf "compile error: %s\n" e.message);
  [%expect {| (Number 3) |}]
;;

let%expect_test "compile error returns Error with message" =
  with_janet_env (fun env ->
    let p = Parser.create () in
    Parser.consume_string p "(def)";
    let ast = Parser.produce p in
    match Compile.compile ast env "test" with
    | Ok _ -> print_endline "unexpected success"
    | Error e -> Printf.printf "got error: %s\n" e.message);
  [%expect {| got error: expected at least 2 arguments to def |}]
;;

let%expect_test "compile + call_exn roundtrip with function" =
  with_janet_env (fun env ->
    let p = Parser.create () in
    Parser.consume_string p "(fn [x] (* x x))";
    let ast = Parser.produce p in
    match Compile.compile ast env "test" with
    | Ok fd ->
      let thunk = Compile.to_function fd in
      let square = Function.call_exn thunk [] in
      let f = Janet_c.C.Functions.janet_unwrap_function square in
      let result = Function.call_exn f [ of_int 7 ] in
      result |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s
    | Error e -> Printf.printf "compile error: %s\n" e.message);
  [%expect {| (Number 49) |}]
;;
