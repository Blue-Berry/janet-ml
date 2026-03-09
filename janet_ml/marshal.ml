module Make (I : Janet_sig.S) = struct
  open! Core
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types
  module Env = Env.Make (I)
  module Janet_buffer = Janet_buffer.Make (I)
  module Janet_table = Janet_table.Make (I)

  let is_nil (v : I.t) =
    match F.janet_type v with
    | T.Nil -> true
    | _ -> false
  ;;

  let is_cfunction (v : I.t) =
    match F.janet_type v with
    | T.CFunction -> true
    | _ -> false
  ;;

  let is_abstract (v : I.t) =
    match F.janet_type v with
    | T.Abstract -> true
    | _ -> false
  ;;

  let copy_table (t : Janet_table.t) : Janet_table.t =
    let out = Janet_table.create (Janet_table.count t) in
    Janet_table.iter t ~f:(fun k v -> Janet_table.put out ~key:k ~value:v);
    out
  ;;

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

  (** Build a reverse registry (value→symbol mapping) from [env], suitable for
      use as the [~rreg] parameter to {!marshal}. This starts from the env's
      [make-image-dict] and augments it with any newly bound cfunctions or
      abstract values. *)
  let make_image_dict (env : Env.t) : Env.t =
    let base =
      Env.Binding.lookup ~env "make-image-dict"
      |> Env.Binding.to_janet
      |> F.janet_unwrap_table
    in
    let out = copy_table base in
    let forward = F.janet_env_lookup env in
    Janet_table.iter forward ~f:(fun sym value ->
      if
        (is_cfunction value || is_abstract value)
        && is_nil (Janet_table.get out ~key:value)
      then Janet_table.put out ~key:value ~value:sym);
    out
  ;;

  (** Build a forward registry (symbol→value mapping) from [env], suitable for
      use as the [~reg] parameter to {!unmarshal}. This starts from the env's
      [load-image-dict] and augments it with any newly bound cfunctions or
      abstract values. *)
  let load_image_dict (env : Env.t) : Env.t =
    let base =
      Env.Binding.lookup ~env "load-image-dict"
      |> Env.Binding.to_janet
      |> F.janet_unwrap_table
    in
    let out = copy_table base in
    let forward = F.janet_env_lookup env in
    Janet_table.iter forward ~f:(fun sym value ->
      if
        (is_cfunction value || is_abstract value) && is_nil (Janet_table.get out ~key:sym)
      then Janet_table.put out ~key:sym ~value);
    out
  ;;

  (** Unmarshal a binary image string back to a Janet value.
      [reg] is the registry (symbol→value mapping, from [load-image-dict])
      used to resolve references during deserialization. When omitted,
      [load_image_dict] from the core env is used. *)
  let unmarshal ?(unsafe = false) ?(no_cycles = false) ?reg image : I.t =
    let flags = (if unsafe then 0x20000 else 0) + if no_cycles then 0x4000 else 0 in
    let reg =
      match reg with
      | Some r -> r
      | None -> load_image_dict (F.janet_core_env None)
    in
    F.janet_unmarshal image (String.length image |> Unsigned.Size_t.of_int) flags reg None
  ;;
end
