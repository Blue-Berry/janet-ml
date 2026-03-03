(** Tests for the Array module. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "Array.create + push/pop" =
  with_janet (fun () ->
    let arr = Array.create 4 in
    Array.push arr (of_int 10);
    Array.push arr (of_int 20);
    Printf.printf "count=%d\n" (Array.count arr);
    let v = Array.pop arr in
    v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s;
    Printf.printf "count after pop=%d\n" (Array.count arr));
  [%expect
    {|
    count=2
    (Number 20)
    count after pop=1
    |}]
;;

let%expect_test "Array.get/set" =
  with_janet (fun () ->
    let arr = Array.create 4 in
    Array.push arr (of_int 1);
    Array.push arr (of_int 2);
    Array.push arr (of_int 3);
    Array.set arr 1 (of_int 99);
    Array.get arr 1 |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s);
  [%expect {| (Number 99) |}]
;;

let%expect_test "Array.ensure + capacity" =
  with_janet (fun () ->
    let arr = Array.create 2 in
    let cap_before = Array.capacity arr in
    Array.ensure arr ~capacity:100 ~growth:2;
    let cap_after = Array.capacity arr in
    Printf.printf "grew: %b\n" (cap_after >= 100 && cap_after > cap_before));
  [%expect {| grew: true |}]
;;

let%expect_test "Array.set_count" =
  with_janet (fun () ->
    let arr = Array.create 4 in
    Array.push arr (of_int 1);
    Array.push arr (of_int 2);
    Array.push arr (of_int 3);
    Array.set_count arr 1;
    Printf.printf "count=%d\n" (Array.count arr));
  [%expect {| count=1 |}]
;;

let%expect_test "Array.of_list / of_array" =
  with_janet (fun () ->
    let arr1 = Array.of_list [ of_int 1; of_int 2; of_int 3 ] in
    Printf.printf "of_list count=%d\n" (Array.count arr1);
    let arr2 = Array.of_array [| of_int 4; of_int 5 |] in
    Printf.printf "of_array count=%d\n" (Array.count arr2));
  [%expect
    {|
    of_list count=3
    of_array count=2
    |}]
;;

let%expect_test "Array.peek" =
  with_janet (fun () ->
    let arr = Array.create 4 in
    Array.push arr (of_int 42);
    Array.push arr (of_int 99);
    Array.peek arr |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s;
    Printf.printf "count unchanged=%d\n" (Array.count arr));
  [%expect
    {|
    (Number 99)
    count unchanged=2
    |}]
;;

let%expect_test "Array.of_janet_array" =
  with_janet (fun () ->
    let arr = Array.of_janet_array [| of_int 10; of_int 20; of_int 30 |] in
    Printf.printf "count=%d\n" (Array.count arr);
    Array.to_list arr
    |> List.iter ~f:(fun v -> v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s));
  [%expect
    {|
    count=3
    (Number 10)
    (Number 20)
    (Number 30)
    |}]
;;

let%expect_test "Array.to_seq" =
  with_janet (fun () ->
    let arr = Array.of_list [ of_int 1; of_int 2; of_int 3 ] in
    Array.to_seq arr
    |> Seq.iter (fun v -> v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s));
  [%expect
    {|
    (Number 1)
    (Number 2)
    (Number 3)
    |}]
;;
