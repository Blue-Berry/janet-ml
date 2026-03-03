(** Tests for the Buffer module. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "Buffer.create + push_string + to_string" =
  with_janet (fun () ->
    let buf = Buffer.create 64 in
    Buffer.push_string buf "hello";
    Printf.printf "%s\n" (Buffer.to_string buf));
  [%expect {| hello |}]
;;

let%expect_test "Buffer.push_bytes + count + contents" =
  with_janet (fun () ->
    let buf = Buffer.create 16 in
    Buffer.push_bytes buf (Bytes.of_string "abc");
    Printf.printf "count=%d\n" (Buffer.count buf);
    Printf.printf "contents=%s\n" (Bytes.to_string (Buffer.contents buf)));
  [%expect
    {|
    count=3
    contents=abc
    |}]
;;

let%expect_test "Buffer.push_u8/u16/u32" =
  with_janet (fun () ->
    let buf = Buffer.create 16 in
    Buffer.push_u8 buf 65;
    Printf.printf "after u8: count=%d\n" (Buffer.count buf);
    Buffer.push_u16 buf 0x4142;
    Printf.printf "after u16: count=%d\n" (Buffer.count buf);
    Buffer.push_u32 buf 0x41424344;
    Printf.printf "after u32: count=%d\n" (Buffer.count buf));
  [%expect
    {|
    after u8: count=1
    after u16: count=3
    after u32: count=7
    |}]
;;

let%expect_test "Buffer.ensure + capacity" =
  with_janet (fun () ->
    let buf = Buffer.create 4 in
    let cap_before = Buffer.capacity buf in
    Buffer.ensure buf ~capacity:100 ~growth:2;
    let cap_after = Buffer.capacity buf in
    Printf.printf "grew: %b\n" (cap_after >= 100 && cap_after > cap_before));
  [%expect {| grew: true |}]
;;

let%expect_test "Buffer.set_count truncation" =
  with_janet (fun () ->
    let buf = Buffer.create 16 in
    Buffer.push_string buf "hello world";
    Printf.printf "before: count=%d\n" (Buffer.count buf);
    Buffer.set_count buf 5;
    Printf.printf
      "after: count=%d contents=%s\n"
      (Buffer.count buf)
      (Buffer.to_string buf));
  [%expect
    {|
    before: count=11
    after: count=5 contents=hello
    |}]
;;
