(** Tests for the Tuple module. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "Tuple.of_list / to_list roundtrip" =
  with_janet (fun () ->
    let items = [ of_int 1; of_int 2; of_int 3 ] in
    let tup = Tuple.of_list items in
    Tuple.to_list tup
    |> List.iter ~f:(fun v -> v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s));
  [%expect
    {|
    (Number 1)
    (Number 2)
    (Number 3)
    |}]
;;

let%expect_test "Tuple.length" =
  with_janet (fun () ->
    let tup = Tuple.of_list [ of_int 10; of_int 20 ] in
    Printf.printf "length=%d\n" (Tuple.length tup));
  [%expect {| length=2 |}]
;;

let%expect_test "Tuple.get by index" =
  with_janet (fun () ->
    let tup = Tuple.of_list [ of_string "a"; of_string "b"; of_string "c" ] in
    Printf.printf "%s\n" (to_string (Tuple.get tup 1)));
  [%expect {| "b" |}]
;;

let%expect_test "Tuple.of_array / to_array" =
  with_janet (fun () ->
    let arr = [| of_int 10; of_int 20; of_int 30 |] in
    let tup = Tuple.of_array arr in
    let result = Tuple.to_array tup in
    Stdlib.Array.iter
      (fun v -> v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s)
      result);
  [%expect
    {|
    (Number 10)
    (Number 20)
    (Number 30)
    |}]
;;

let%expect_test "Tuple.to_seq" =
  with_janet (fun () ->
    let tup = Tuple.of_list [ of_int 1; of_int 2; of_int 3 ] in
    Tuple.to_seq tup
    |> Seq.iter (fun v -> v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s));
  [%expect
    {|
    (Number 1)
    (Number 2)
    (Number 3)
    |}]
;;
