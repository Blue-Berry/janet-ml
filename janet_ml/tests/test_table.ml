(** Tests for Table/Struct iteration, construction, and folding. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let kw s = Unwrapped.to_janet (Unwrapped.Keyword s)
let num n = Unwrapped.to_janet (Unwrapped.Number n)
let str s = Unwrapped.to_janet (Unwrapped.String s)

let sort_pairs pairs =
  List.sort pairs ~compare:(fun (k1, _) (k2, _) ->
    String.compare (to_string k1) (to_string k2))
;;

let%expect_test "Table.of_pairs creates a table accessible from Janet" =
  with_janet_env (fun env ->
    let tbl = Table.of_pairs [ kw "name", str "Alice"; kw "age", num 30.0 ] in
    Env.def env "person" (Table.wrap tbl);
    Janet_ml.dostring_exn ~env "(get person :name)"
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect {| (String Alice) |}]
;;

let%expect_test "Table.of_pairs from an empty list creates an empty table" =
  with_janet_env (fun _env ->
    let tbl = Table.of_pairs [] in
    let n = Table.count tbl in
    if n = 0 then print_endline "empty" else Printf.printf "count=%d\n" n);
  [%expect {| empty |}]
;;

let%expect_test "Table.iter visits every key-value pair" =
  with_janet_env (fun _env ->
    let tbl = Table.of_pairs [ kw "a", num 1.0; kw "b", num 2.0; kw "c", num 3.0 ] in
    let collected = ref [] in
    Table.iter tbl ~f:(fun k v -> collected := (to_string k, to_string v) :: !collected);
    List.sort !collected ~compare:[%compare: string * string]
    |> List.iter ~f:(fun (k, v) -> Printf.printf "%s=%s\n" k v));
  [%expect
    {|
    :a=1
    :b=2
    :c=3
    |}]
;;

let%expect_test "Table.iter on an empty table calls f zero times" =
  with_janet_env (fun _env ->
    let count = ref 0 in
    Table.iter (Table.of_pairs []) ~f:(fun _k _v -> incr count);
    Printf.printf "called %d times\n" !count);
  [%expect {| called 0 times |}]
;;

let%expect_test "Table.fold sums all numeric values" =
  with_janet_env (fun _env ->
    let tbl = Table.of_pairs [ kw "x", num 10.0; kw "y", num 20.0; kw "z", num 12.0 ] in
    let total =
      Table.fold tbl ~init:0.0 ~f:(fun acc _k v ->
        match Unwrapped.of_janet v with
        | Unwrapped.Number n -> acc +. n
        | _ -> acc)
    in
    Printf.printf "%.0f\n" total);
  [%expect {| 42 |}]
;;

let%expect_test "Table.fold collects all keys" =
  with_janet_env (fun _env ->
    let tbl = Table.of_pairs [ kw "p", num 1.0; kw "q", num 2.0 ] in
    Table.fold tbl ~init:[] ~f:(fun acc k _v -> to_string k :: acc)
    |> List.sort ~compare:String.compare
    |> List.iter ~f:print_endline);
  [%expect
    {|
    :p
    :q
    |}]
;;

let%expect_test "Table.to_pairs round-trips" =
  with_janet_env (fun _env ->
    Table.of_pairs [ kw "alpha", num 1.0; kw "beta", num 2.0 ]
    |> Table.to_pairs
    |> sort_pairs
    |> List.iter ~f:(fun (k, v) -> Printf.printf "%s -> %s\n" (to_string k) (to_string v)));
  [%expect
    {|
    :alpha -> 1
    :beta -> 2
    |}]
;;

let%expect_test "Table built by Janet code is iterable from OCaml" =
  with_janet_env (fun env ->
    (* Use @{} syntax for a mutable table, {} creates an immutable struct *)
    let tbl = Janet_ml.dostring_exn ~env "@{:one 1 :two 2 :three 3}" |> Table.unwrap in
    let sum =
      Table.fold tbl ~init:0.0 ~f:(fun acc _k v ->
        match Unwrapped.of_janet v with
        | Unwrapped.Number n -> acc +. n
        | _ -> acc)
    in
    Printf.printf "%.0f\n" sum);
  [%expect {| 6 |}]
;;

let%expect_test "Struct.iter visits every key-value pair" =
  with_janet_env (fun env ->
    let st = Janet_ml.dostring_exn ~env "{:r 255 :g 128 :b 0}" |> Struct.unwrap in
    let collected = ref [] in
    Struct.iter st ~f:(fun k v -> collected := (to_string k, to_string v) :: !collected);
    List.sort !collected ~compare:[%compare: string * string]
    |> List.iter ~f:(fun (k, v) -> Printf.printf "%s=%s\n" k v));
  [%expect
    {|
    :b=0
    :g=128
    :r=255
    |}]
;;

let%expect_test "Struct.fold sums values" =
  with_janet_env (fun env ->
    let st = Janet_ml.dostring_exn ~env "{:x 3 :y 4}" |> Struct.unwrap in
    let sum =
      Struct.fold st ~init:0.0 ~f:(fun acc _k v ->
        match Unwrapped.of_janet v with
        | Unwrapped.Number n -> acc +. n
        | _ -> acc)
    in
    Printf.printf "%.0f\n" sum);
  [%expect {| 7 |}]
;;

let%expect_test "Struct.to_pairs returns all pairs" =
  with_janet_env (fun _env ->
    Struct.of_pairs [ kw "hello", str "world"; kw "n", num 42.0 ]
    |> Struct.to_pairs
    |> sort_pairs
    |> List.iter ~f:(fun (k, v) -> Printf.printf "%s -> %s\n" (to_string k) (to_string v)));
  [%expect
    {|
    :hello -> "world"
    :n -> 42
    |}]
;;
