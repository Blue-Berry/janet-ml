(** Tests for Janet.with_root – GC rooting bracket. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let%expect_test "with_root returns the value of f" =
  with_janet_env (fun env ->
    let v = Janet_ml.dostring_exn ~env "(+ 1 2)" in
    with_root v ~f:(fun rooted -> Unwrapped.of_janet rooted)
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (Number 3) |}]
;;

let%expect_test "with_root value survives a subsequent allocation" =
  with_janet_env (fun env ->
    let v = Janet_ml.dostring_exn ~env "(+ 10 32)" in
    with_root v ~f:(fun rooted ->
      let _ = Janet_ml.dostring_exn ~env {|(string "hello")|} in
      rooted |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s));
  [%expect {| (Number 42) |}]
;;

let%expect_test "with_root value survives many allocating operations" =
  with_janet_env (fun env ->
    let v = Janet_ml.dostring_exn ~env "(* 6 7)" in
    with_root v ~f:(fun rooted ->
      for _ = 1 to 5 do
        ignore (Janet_ml.dostring_exn ~env "(array 1 2 3 4 5)")
      done;
      rooted |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s));
  [%expect {| (Number 42) |}]
;;

let%expect_test "nested with_root roots both values independently" =
  with_janet_env (fun env ->
    let v1 = Janet_ml.dostring_exn ~env "(+ 1 0)" in
    let v2 = Janet_ml.dostring_exn ~env "(+ 2 0)" in
    with_root v1 ~f:(fun r1 ->
      with_root v2 ~f:(fun r2 ->
        ignore (Janet_ml.dostring_exn ~env "(array 9 8 7)");
        let n1 = r1 |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> Sexp.to_string_hum in
        let n2 = r2 |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> Sexp.to_string_hum in
        Printf.printf "%s %s\n" n1 n2)));
  [%expect {| (Number 1) (Number 2) |}]
;;

let%expect_test "with_root unroots even when f raises" =
  with_janet_env (fun env ->
    let v = Janet_ml.dostring_exn ~env "(+ 5 5)" in
    (try with_root v ~f:(fun _rooted -> raise (Failure "inner failure")) with
     | Failure msg -> Printf.printf "caught: %s\n" msg);
    Janet_ml.dostring_exn ~env "(+ 1 1)"
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect
    {|
    caught: inner failure
    (Number 2)
    |}]
;;

let%expect_test "with_root on a table keeps it alive" =
  with_janet_env (fun _env ->
    (* Build a real mutable table from OCaml — @{} syntax *)
    let tbl =
      Table.of_pairs
        [ ( Unwrapped.to_janet (Unwrapped.Keyword "answer")
          , Unwrapped.to_janet (Unwrapped.Number 42.0) )
        ]
    in
    let tbl_val = Table.wrap tbl in
    with_root tbl_val ~f:(fun rooted ->
      (* Allocate noise to stress the GC *)
      let noise =
        Table.of_pairs
          [ ( Unwrapped.to_janet (Unwrapped.Keyword "x")
            , Unwrapped.to_janet (Unwrapped.Number 1.0) )
          ; ( Unwrapped.to_janet (Unwrapped.Keyword "y")
            , Unwrapped.to_janet (Unwrapped.Number 2.0) )
          ]
      in
      ignore noise;
      Table.get
        (Table.unwrap rooted)
        ~key:(Unwrapped.to_janet (Unwrapped.Keyword "answer"))
      |> Unwrapped.of_janet
      |> Unwrapped.sexp_of_t
      |> print_s));
  [%expect {| (Number 42) |}]
;;

let%expect_test "with_root on a string keeps it readable" =
  with_janet_env (fun env ->
    let s_val = Janet_ml.dostring_exn ~env {|(string "persistent-string")|} in
    with_root s_val ~f:(fun rooted ->
      ignore (Janet_ml.dostring_exn ~env "(array 0 0 0)");
      rooted |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> print_s));
  [%expect {| (String persistent-string) |}]
;;
