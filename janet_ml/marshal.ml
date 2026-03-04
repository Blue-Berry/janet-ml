module Make (I : Janet_sig.S) = struct
  open! Core
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types
  module Env = Env.Make (I)
  module Janet_buffer = Janet_buffer.Make (I)

  (** Marshal a Janet value to a binary image string.
      [rreg] is the reverse registry (value→symbol mapping) used to serialize
      references to known values. Obtain it from the [make-image-dict] binding
      in a Janet environment. When omitted, no reverse registry is used. *)
  let marshal
        ?(unsafe = false)
        ?(no_cycles = false)
        ?(max_size = 512)
        ?rreg
        (target : I.t)
    =
    let flags = (if unsafe then 0x20000 else 0) + if no_cycles then 0x4000 else 0 in
    let image = Janet_buffer.create max_size in
    F.janet_marshal image target rreg flags;
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
    marshal ~unsafe ~no_cycles ~rreg:env target
  ;;

  (** Unmarshal a binary image string back to a Janet value.
      [reg] is the registry (symbol→value mapping) used to resolve references
      during deserialization. When omitted, [janet_env_lookup(janet_core_env())]
      is used, which handles all core symbols. *)
  let unmarshal ?(unsafe = false) ?(no_cycles = false) ?reg image : I.t =
    let flags = (if unsafe then 0x20000 else 0) + if no_cycles then 0x4000 else 0 in
    let lookup =
      match reg with
      | Some r -> r
      | None -> F.janet_env_lookup (F.janet_core_env None)
    in
    F.janet_unmarshal
      image
      (String.length image |> Unsigned.Size_t.of_int)
      flags
      lookup
      None
  ;;
end
