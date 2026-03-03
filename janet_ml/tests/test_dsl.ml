(** Tests for persistent DSL base environment + per-call child env isolation. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let ocaml_counter_calls = ref 0

module Demo = Dsl.Make (struct
    let janet_lib =
      {|
(var dsl-load-count-var 0)
(set dsl-load-count-var (+ dsl-load-count-var 1))

(defn dsl-load-count [] dsl-load-count-var)
(defn dsl-add-one [x] (+ x 1))
|}
    ;;

    type fun_t =
      { name : string
      ; f : Janet.t array -> Janet.t
      }

    let ext_funs =
      [ { name = "ocaml-counter"
        ; f =
            (fun _args ->
              incr ocaml_counter_calls;
              of_int !ocaml_counter_calls)
        }
      ]
    ;;
  end)

let%expect_test "Dsl base setup runs once and is reused across calls" =
  ocaml_counter_calls := 0;
  Demo.with_env (fun env ->
    Janet_ml.dostring_exn ~env "(dsl-load-count)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s;
    Janet_ml.dostring_exn ~env "(ocaml-counter)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  Demo.with_env (fun env ->
    Janet_ml.dostring_exn ~env "(dsl-load-count)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s;
    Janet_ml.dostring_exn ~env "(ocaml-counter)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect
    {|
    (Number 1)
    (Number 1)
    (Number 1)
    (Number 2)
    |}]
;;

let%expect_test "Dsl.with_env gets a fresh child env each call" =
  Demo.with_env (fun env ->
    ignore (Janet_ml.dostring_exn ~env "(def ephemeral 42)" ~source_path:None);
    match Env.Binding.lookup ~env "ephemeral" with
    | Env.Binding.Def _ -> print_endline "present in call 1"
    | _ -> print_endline "missing in call 1");
  Demo.with_env (fun env ->
    match Env.Binding.lookup ~env "ephemeral" with
    | Env.Binding.None -> print_endline "absent in call 2"
    | _ -> print_endline "leaked into call 2");
  [%expect
    {|
    present in call 1
    absent in call 2
    |}]
;;

let%expect_test "Dsl base bindings survive allocation pressure across calls" =
  Demo.with_env (fun env ->
    for _ = 1 to 2000 do
      ignore (Janet_ml.dostring_exn ~env "@[1 2 3 4 5]" ~source_path:None)
    done;
    Janet_ml.dostring_exn ~env "(dsl-add-one 41)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  Demo.with_env (fun env ->
    for _ = 1 to 2000 do
      ignore (Janet_ml.dostring_exn ~env "@[9 8 7 6 5]" ~source_path:None)
    done;
    Janet_ml.dostring_exn ~env "(dsl-add-one 41)" ~source_path:None
    |> Unwrapped.of_janet
    |> Unwrapped.sexp_of_t
    |> print_s);
  [%expect
    {|
    (Number 42)
    (Number 42)
    |}]
;;
