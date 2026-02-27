open! Core
module F = Janet_c.C.Functions
module T = Janet_c.C.Types

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
