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
  janet_dostring ~env src ~source_path:(Some "test")
  |> Janet_type.of_janet
  |> Janet_type.sexp_of_t
  |> print_s;
  [%expect {| (Number 5) |}]
;;

(* janet_resolve(env, janet_csymbol("load-image-dict"), &lidv); *)

let%expect_test "Test resolve and call janet function" =
  janet_init ();
  let src =
    {|
(defn test [a b] (+ a b))
(defn main [&] (test 1 2))
|}
  in
  let env = janet_core_env None in
  let _out = janet_dostring ~env src ~source_path:(Some "test") in
  let main = Janet.create_ptr () in
  let _b_type = F.janet_resolve env (F.janet_csymbol "main") main in
  (match Janet_type.of_janet @@ Janet.of_ptr main with
   | Janet_type.Function main ->
     let f = Janet_fiber.create main ~capacity:64 ~argv:[] in
     Janet_fiber.continue f (Janet.create ())
     |> snd
     |> Janet_type.of_janet
     |> Janet_type.sexp_of_t
     |> Sexp.to_string_hum
   | _ -> "Not a function")
  |> print_endline;
  [%expect {| (Number 3) |}]
;;

let%expect_test "Test resolve and call janet function" =
  janet_init ();
  let src =
    {|
(defn test [a b] (+ a b))
(defn main [&] (test 1 2))
|}
  in
  let env = janet_core_env None in
  let _out = janet_dostring ~env src ~source_path:(Some "test") in
  let main = Janet.create_ptr () in
  let _b_type = F.janet_resolve env (F.janet_csymbol "main") main in
  let image = F.janet_buffer (Int32.of_int_exn 512) in
  F.janet_marshal image (Janet.of_ptr main) (Some env) 0;
  let main =
    F.janet_unmarshal
      (Janet_buffer.to_string image)
      (Janet_buffer.count image |> Unsigned.Size_t.of_int)
      0
      (F.janet_env_lookup (F.janet_core_env None))
      None
  in
  (match Janet_type.of_janet main with
   | Janet_type.Function main ->
     let f = Janet_fiber.create main ~capacity:64 ~argv:[] in
     Janet_fiber.continue f (Janet.create ())
     |> snd
     |> Janet_type.of_janet
     |> Janet_type.sexp_of_t
     |> Sexp.to_string_hum
   | _ -> "Not a function")
  |> print_endline;
  [%expect {| (Number 3) |}]
;;
