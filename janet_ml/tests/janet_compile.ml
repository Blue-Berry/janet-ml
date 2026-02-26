open! Core
open Janet_ml

let%expect_test "Test compile janet" =
  janet_init ();
  let src =
    {|
(defn test [a b] (+ a b))

(+ 1 2)
(+ 2 2)
(test 3 2)
|}
  in
  let env = janet_core_env None in
  match Janet_ml.Janet_parser.compile_string ~env src ~source:"test" with
  | Error e ->
    Format.sprintf "Failed to compile: %s; %d:%d" e.message e.line e.column
    |> print_endline
  | Ok fs ->
    List.map
      ~f:(fun f ->
        let fiber = Janet_fiber.create f ~capacity:64 ~argv:[] in
        Janet_fiber.set_env fiber env;
        let res = Janet_fiber.continue fiber (Janet_c.C.Functions.janet_wrap_nil ()) in
        res)
      fs
    |> List.iter ~f:(fun (_, j) ->
      Janet_type.of_janet j |> Janet_type.sexp_of_t |> print_s);
    [%expect
      {|
      (Function janet_function)
      (Number 3)
      (Number 4)
      (String "nil called with 2 arguments, possibly expected 1")
      |}]
;;

let%expect_test "Test compile janet" =
  janet_init ();
  let src =
    {|
(defn test [a b] (+ a b))

(+ 1 2)
(+ 2 2)
(test 3 2)
|}
  in
  let env = janet_core_env None in
  janet_dostring ~env src ~source_path:(Some "test")
  |> Janet_type.of_janet
  |> Janet_type.sexp_of_t
  |> print_s;
  [%expect {| (Number 5) |}]
;;
