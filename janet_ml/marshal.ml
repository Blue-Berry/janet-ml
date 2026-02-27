open! Core
module F = Janet_c.C.Functions
module T = Janet_c.C.Types

(* let%expect_test "Test resolve and call janet function" = *)
(*   janet_init (); *)
(*   let src = *)
(*     {| *)
(* (defn test [a b] (+ a b)) *)
(* (defn main [&] (test 1 2)) *)
(* |} *)
(*   in *)
(*   let env = janet_core_env None in *)
(*   let _out = janet_dostring ~env src ~source_path:(Some "test") in *)
(*   let main = Janet.create_ptr () in *)
(*   let _b_type = F.janet_resolve env (F.janet_csymbol "main") main in *)
(*   let image = F.janet_buffer (Int32.of_int_exn 512) in *)
(*   F.janet_marshal image (Janet.of_ptr main) (Some env) 0; *)
(*   let main = *)
(*     F.janet_unmarshal *)
(*       (Janet_buffer.to_string image) *)
(*       (Janet_buffer.count image |> Unsigned.Size_t.of_int) *)
(*       0 *)
(*       (F.janet_env_lookup (F.janet_core_env None)) *)
(*       None *)
(*   in *)
(*   (match Janet_type.of_janet main with *)
(*    | Janet_type.Function main -> *)
(*      let f = Janet_fiber.create main ~capacity:64 ~argv:[] in *)
(*      Janet_fiber.continue f (Janet.create ()) *)
(*      |> snd *)
(*      |> Janet_type.of_janet *)
(*      |> Janet_type.sexp_of_t *)
(*      |> Sexp.to_string_hum *)
(*    | _ -> "Not a function") *)
(*   |> print_endline; *)
(*   [%expect {| (Number 3) |}] *)
(* ;; *)

(* JANET_API void janet_marshal( *)
(*     JanetBuffer *buf, *)
(*     Janet x, *)
(*     JanetTable *rreg, *)
(*     int flags); *)
(* JANET_API Janet janet_unmarshal( *)
(*     const uint8_t *bytes, *)
(*     size_t len, *)
(*     int flags, *)
(*     JanetTable *reg, *)
(*     const uint8_t **next); *)
(* JANET_API JanetTable *janet_env_lookup(JanetTable *env); *)
(* JANET_API void janet_env_lookup_into(JanetTable *renv, JanetTable *env, const char *prefix, int recurse); *)

let marshal
      ?(unsafe = false)
      ?(no_cycles = false)
      ?(max_size = 512)
      ~(env : Env.t option)
      (target : Janet.t)
  =
  (* UNSAFE 0x20000 *)
  (* NO_CYCLES 0x40000 *)
  let flags = (if unsafe then 0x20000 else 0) + if no_cycles then 0x4000 else 0 in
  let image = Janet_buffer.create max_size in
  F.janet_marshal image target env flags;
  Janet_buffer.to_string image
;;

let marshal_symbol
      ?(unsafe = false)
      ?(no_cycles = false)
      ?(max_size = 512)
      ~(env : Env.t)
      (target : string)
  =
  let target = Env.Binding.lookup ~env target |> Env.Binding.to_janet in
  marshal ~unsafe ~no_cycles ~env:(Some env) target
;;

(* let main = *)
(*   F.janet_unmarshal *)
(*     (Janet_buffer.to_string image) *)
(*     (Janet_buffer.count image |> Unsigned.Size_t.of_int) *)
(*     0 *)
(*     (F.janet_env_lookup (F.janet_core_env None)) *)
(*     None *)

(* TODO: add merging lookup with janet_env_lookup_into *)
let unmarshal ?(unsafe = false) ?(no_cycles = false) ?(env : Env.t option = None) image
  : Janet.t
  =
  let flags = (if unsafe then 0x20000 else 0) + if no_cycles then 0x4000 else 0 in
  let env =
    match env with
    | Some env -> env
    | None -> F.janet_core_env None
  in
  let lookup = F.janet_env_lookup env in
  F.janet_unmarshal
    image
    (String.length image |> Unsigned.Size_t.of_int)
    flags
    lookup
    None
;;
