(** Tests for Janet.to_string, Janet.to_string_value, and Janet.pretty. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "to_string of a number" =
  with_janet_env (fun _env ->
    Unwrapped.to_janet (Unwrapped.Number 42.0) |> to_string |> print_endline);
  [%expect {| 42 |}]
;;

let%expect_test "to_string of a float number" =
  with_janet_env (fun _env ->
    Unwrapped.to_janet (Unwrapped.Number 3.14) |> to_string |> print_endline);
  [%expect {| 3.14 |}]
;;

let%expect_test "to_string of nil" =
  with_janet_env (fun _env ->
    Unwrapped.to_janet Unwrapped.Nil |> to_string |> print_endline);
  [%expect {| nil |}]
;;

let%expect_test "to_string of booleans" =
  with_janet_env (fun _env ->
    Printf.printf
      "%s %s\n"
      (Unwrapped.to_janet (Unwrapped.Boolean true) |> to_string)
      (Unwrapped.to_janet (Unwrapped.Boolean false) |> to_string));
  [%expect {| true false |}]
;;

let%expect_test "to_string of a string wraps it in quotes" =
  with_janet_env (fun _env ->
    Unwrapped.to_janet (Unwrapped.String "hello") |> to_string |> print_endline);
  [%expect {| "hello" |}]
;;

let%expect_test "to_string of a keyword" =
  with_janet_env (fun _env ->
    Unwrapped.to_janet (Unwrapped.Keyword "my-kw") |> to_string |> print_endline);
  [%expect {| :my-kw |}]
;;

let%expect_test "to_string of a symbol" =
  with_janet_env (fun _env ->
    Unwrapped.to_janet (Unwrapped.Symbol "my-sym") |> to_string |> print_endline);
  [%expect {| my-sym |}]
;;

let%expect_test "to_string of an array produces a pointer description" =
  with_janet_env (fun env ->
    (* janet_description/janet_to_string render arrays as <array 0x...>.
       Use janet_pretty for human-readable output. *)
    let v = Janet_ml.dostring_exn ~env "@[1 2 3]" in
    Printf.printf "starts-with-at=%b\n" (String.is_prefix (to_string v) ~prefix:"<array"));
  [%expect {| starts-with-at=true |}]
;;

let%expect_test "pretty of an array gives readable output" =
  with_janet_env (fun env ->
    Janet_ml.dostring_exn ~env "@[1 2 3]" |> pretty |> print_endline);
  [%expect {| @[1 2 3] |}]
;;

let%expect_test "pretty of a tuple gives readable output" =
  with_janet_env (fun env ->
    Janet_ml.dostring_exn ~env {|["a" 1]|} |> pretty |> print_endline);
  [%expect {| ("a" 1) |}]
;;

let%expect_test "pretty of a struct starts with {" =
  with_janet_env (fun env ->
    let v = Janet_ml.dostring_exn ~env "{:x 1 :y 2}" in
    Printf.printf "starts-with-brace=%b\n" (String.is_prefix (pretty v) ~prefix:"{"));
  [%expect {| starts-with-brace=true |}]
;;

let%expect_test "to_string_value of a string gives raw content without quotes" =
  with_janet_env (fun _env ->
    Unwrapped.to_janet (Unwrapped.String "raw content")
    |> to_string_value
    |> print_endline);
  [%expect {| raw content |}]
;;

let%expect_test "to_string_value of a number" =
  with_janet_env (fun _env ->
    Unwrapped.to_janet (Unwrapped.Number 100.0) |> to_string_value |> print_endline);
  [%expect {| 100 |}]
;;

let%expect_test "to_string_value of a keyword" =
  with_janet_env (fun _env ->
    Unwrapped.to_janet (Unwrapped.Keyword "tag") |> to_string_value |> print_endline);
  [%expect {| tag |}]
;;

let%expect_test "pretty output is non-empty" =
  with_janet_env (fun env ->
    let v = Janet_ml.dostring_exn ~env "{:a 1}" in
    Printf.printf "non-empty=%b\n" (not (String.is_empty (pretty v))));
  [%expect {| non-empty=true |}]
;;

let%expect_test "pretty of a nested structure contains the leaf values" =
  with_janet_env (fun env ->
    let v = Janet_ml.dostring_exn ~env "{:data [1 2 3]}" in
    let s = pretty ~depth:4 v in
    Printf.printf
      "has-1=%b has-2=%b has-3=%b\n"
      (String.is_substring s ~substring:"1")
      (String.is_substring s ~substring:"2")
      (String.is_substring s ~substring:"3"));
  [%expect {| has-1=true has-2=true has-3=true |}]
;;

let%expect_test "pretty depth:0 produces shorter or equal output than depth:4" =
  with_janet_env (fun env ->
    let v = Janet_ml.dostring_exn ~env "{:nested {:deep :value}}" in
    let shallow = pretty ~depth:0 v in
    let deep = pretty ~depth:4 v in
    Printf.printf "shallow-lte=%b\n" (String.length shallow <= String.length deep));
  [%expect {| shallow-lte=true |}]
;;
