(** Tests for the Parser module. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "Parser.create starts in Root status" =
  with_janet (fun () ->
    let p = Parser.create () in
    match Parser.status p with
    | Parser.Root -> print_endline "Root"
    | _ -> print_endline "other");
  [%expect {| Root |}]
;;

let%expect_test "Parser.consume_string + produce parses a simple expression" =
  with_janet (fun () ->
    let p = Parser.create () in
    Parser.consume_string p "(+ 1 2)";
    assert (Parser.has_more p);
    let v = Parser.produce p in
    v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s);
  [%expect {| (Tuple ((Symbol +) (Number 1) (Number 2))) |}]
;;

let%expect_test "Parser with partial input is Pending" =
  with_janet (fun () ->
    let p = Parser.create () in
    Parser.consume_string p "(+ 1";
    match Parser.status p with
    | Parser.Pending -> print_endline "Pending"
    | _ -> print_endline "other");
  [%expect {| Pending |}]
;;

let%expect_test "Parser error on bad input" =
  with_janet (fun () ->
    let p = Parser.create () in
    Parser.consume_string p ")";
    match Parser.status p with
    | Parser.Error ->
      (match Parser.error p with
       | Some msg -> Printf.printf "error: %s\n" msg
       | None -> print_endline "error with no message")
    | _ -> print_endline "not an error");
  [%expect {| error: unexpected closing delimiter ) |}]
;;

let%expect_test "Parser.line and Parser.column track position" =
  with_janet (fun () ->
    let p = Parser.create () in
    Parser.consume_string p "hello";
    Printf.printf "line=%d col=%d\n" (Parser.line p) (Parser.column p));
  [%expect {| line=1 col=5 |}]
;;

let%expect_test "Parser.eof + flush completes pending parse" =
  with_janet (fun () ->
    let p = Parser.create () in
    Parser.consume_string p "42";
    Parser.eof p;
    assert (Parser.has_more p);
    let v = Parser.produce p in
    v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s);
  [%expect {| (Number 42) |}]
;;

let%expect_test "Parser produces multiple values from one string" =
  with_janet (fun () ->
    let p = Parser.create () in
    Parser.consume_string p "1 2 3";
    Parser.eof p;
    let values = ref [] in
    while Parser.has_more p do
      values := Parser.produce p :: !values
    done;
    List.rev !values
    |> List.iter ~f:(fun v -> v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s));
  [%expect
    {|
    (Number 1)
    (Number 2)
    (Number 3)
    |}]
;;
