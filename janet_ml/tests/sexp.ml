open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "round-trip number" =
  init ();
  let sexp = Unwrapped.sexp_of_t (Number 42.0) in
  print_s sexp;
  [%expect {| (Number 42) |}];
  let v = Unwrapped.t_of_sexp sexp in
  Unwrapped.sexp_of_t v |> print_s;
  [%expect {| (Number 42) |}]
;;

let%expect_test "round-trip nil" =
  init ();
  let sexp = Unwrapped.sexp_of_t Nil in
  print_s sexp;
  [%expect {| Nil |}];
  let v = Unwrapped.t_of_sexp sexp in
  Unwrapped.sexp_of_t v |> print_s;
  [%expect {| Nil |}]
;;

let%expect_test "round-trip boolean" =
  init ();
  let sexp = Unwrapped.sexp_of_t (Boolean true) in
  print_s sexp;
  [%expect {| (Boolean true) |}];
  let v = Unwrapped.t_of_sexp sexp in
  Unwrapped.sexp_of_t v |> print_s;
  [%expect {| (Boolean true) |}]
;;

let%expect_test "round-trip string" =
  init ();
  let sexp = Unwrapped.sexp_of_t (String "hello world") in
  print_s sexp;
  [%expect {| (String "hello world") |}];
  let v = Unwrapped.t_of_sexp sexp in
  Unwrapped.sexp_of_t v |> print_s;
  [%expect {| (String "hello world") |}]
;;

let%expect_test "round-trip symbol" =
  init ();
  let sexp = Unwrapped.sexp_of_t (Symbol "my-sym") in
  print_s sexp;
  [%expect {| (Symbol my-sym) |}];
  let v = Unwrapped.t_of_sexp sexp in
  Unwrapped.sexp_of_t v |> print_s;
  [%expect {| (Symbol my-sym) |}]
;;

let%expect_test "round-trip keyword" =
  init ();
  let sexp = Unwrapped.sexp_of_t (Keyword "my-kw") in
  print_s sexp;
  [%expect {| (Keyword my-kw) |}];
  let v = Unwrapped.t_of_sexp sexp in
  Unwrapped.sexp_of_t v |> print_s;
  [%expect {| (Keyword my-kw) |}]
;;

let%expect_test "round-trip buffer" =
  init ();
  let sexp = Unwrapped.sexp_of_t (Buffer (Bytes.of_string "buf data")) in
  print_s sexp;
  [%expect {| (Buffer "buf data") |}];
  let v = Unwrapped.t_of_sexp sexp in
  Unwrapped.sexp_of_t v |> print_s;
  [%expect {| (Buffer "buf data") |}]
;;

let%expect_test "round-trip array" =
  init ();
  let sexp = Unwrapped.sexp_of_t (Array [ Number 1.0; String "two"; Nil ]) in
  print_s sexp;
  [%expect {| (Array ((Number 1) (String two) Nil)) |}];
  let v = Unwrapped.t_of_sexp sexp in
  Unwrapped.sexp_of_t v |> print_s;
  [%expect {| (Array ((Number 1) (String two) Nil)) |}]
;;

let%expect_test "round-trip tuple" =
  init ();
  let sexp = Unwrapped.sexp_of_t (Tuple [ Boolean false; Number 3.14 ]) in
  print_s sexp;
  [%expect {| (Tuple ((Boolean false) (Number 3.14))) |}];
  let v = Unwrapped.t_of_sexp sexp in
  Unwrapped.sexp_of_t v |> print_s;
  [%expect {| (Tuple ((Boolean false) (Number 3.14))) |}]
;;

let%expect_test "round-trip nested array" =
  init ();
  let sexp =
    Unwrapped.sexp_of_t (Array [ Array [ Number 1.0; Number 2.0 ]; Keyword "nested" ])
  in
  print_s sexp;
  [%expect {| (Array ((Array ((Number 1) (Number 2))) (Keyword nested))) |}];
  let v = Unwrapped.t_of_sexp sexp in
  Unwrapped.sexp_of_t v |> print_s;
  [%expect {| (Array ((Array ((Number 1) (Number 2))) (Keyword nested))) |}]
;;

let%expect_test "Janet.t round-trip via sexp" =
  init ();
  let env = Env.core_env () in
  let result = Janet_ml.dostring_exn ~env {|(+ 1 2)|} ~source_path:"test" in
  let sexp = sexp_of_t result in
  print_s sexp;
  [%expect {| (Number 3) |}];
  let result' = t_of_sexp sexp in
  Unwrapped.of_janet result' |> Unwrapped.sexp_of_t |> print_s;
  [%expect {| (Number 3) |}]
;;

let%expect_test "Array.t round-trip via sexp" =
  init ();
  let arr = Array.of_list [ Unwrapped.to_janet (Number 10.0); Unwrapped.to_janet Nil ] in
  let sexp = Array.sexp_of_t arr in
  print_s sexp;
  [%expect {| (Array ((Number 10) Nil)) |}];
  let arr' = Array.t_of_sexp sexp in
  Array.sexp_of_t arr' |> print_s;
  [%expect {| (Array ((Number 10) Nil)) |}]
;;

let%expect_test "Buffer.t round-trip via sexp" =
  init ();
  let buf = Buffer.create 16 in
  Buffer.push_string buf "test data";
  let sexp = Buffer.sexp_of_t buf in
  print_s sexp;
  [%expect {| (Buffer "test data") |}];
  let buf' = Buffer.t_of_sexp sexp in
  Buffer.sexp_of_t buf' |> print_s;
  [%expect {| (Buffer "test data") |}]
;;

let%expect_test "Tuple.t round-trip via sexp" =
  init ();
  let tup =
    Tuple.of_list [ Unwrapped.to_janet (Number 1.0); Unwrapped.to_janet (String "hi") ]
  in
  let sexp = Tuple.sexp_of_t tup in
  print_s sexp;
  [%expect {| (Tuple ((Number 1) (String hi))) |}];
  let tup' = Tuple.t_of_sexp sexp in
  Tuple.sexp_of_t tup' |> print_s;
  [%expect {| (Tuple ((Number 1) (String hi))) |}]
;;
