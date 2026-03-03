module Make (I : Janet_sig.S) = struct
  open! Core
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types
  module Janet_struct = Janet_struct.Make (I)
  module Janet_kv = Janet_kv.Make (I)

  type t = Type.table

  let to_janet : t -> I.t = F.janet_wrap_table
  let sexp_of_t t = Sexp.Atom (to_janet t |> I.to_string)
  let create capacity : t = F.janet_table (Int32.of_int_exn capacity)
  let get (tbl : t) ~(key : I.t) : I.t = F.janet_table_get tbl key
  let rawget (tbl : t) ~(key : I.t) : I.t = F.janet_table_rawget tbl key
  let remove (tbl : t) ~(key : I.t) : I.t = F.janet_table_remove tbl key
  let put (tbl : t) ~(key : I.t) ~(value : I.t) = F.janet_table_put tbl key value

  let of_pairs (pairs : (I.t * I.t) list) : t =
    let tbl = create (List.length pairs) in
    List.iter pairs ~f:(fun (k, v) -> put tbl ~key:k ~value:v);
    tbl
  ;;

  let to_struct (tbl : t) : Janet_struct.t =
    let data = F.janet_table_to_struct tbl in
    let offset = Ctypes.sizeof T.Janet_Struct.head in
    let p = Ctypes.from_voidp Ctypes.char (Ctypes.to_voidp data) in
    Ctypes.from_voidp T.Janet_Struct.head (Ctypes.to_voidp Ctypes.(p +@ -offset))
  ;;

  let merge_table (tbl : t) (other : t) = F.janet_table_merge_table tbl other

  let merge_struct (tbl : t) (other : Janet_struct.t) =
    let offset = Ctypes.sizeof T.Janet_Struct.head in
    let p = Ctypes.from_voidp Ctypes.char (Ctypes.to_voidp other) in
    let data = Ctypes.from_voidp T.janet_kv (Ctypes.to_voidp Ctypes.(p +@ offset)) in
    F.janet_table_merge_struct tbl data
  ;;

  let find (tbl : t) ~(key : I.t) : Janet_kv.t = F.janet_table_find tbl key
  let count (tbl : t) = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.count |> Int32.to_int_exn

  let capacity (tbl : t) =
    Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.capacity |> Int32.to_int_exn
  ;;

  let proto (tbl : t) : t option = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.proto
  let wrap (tbl : t) : I.t = F.janet_wrap_table tbl
  let unwrap (j : I.t) : t = F.janet_unwrap_table j

  (* Iterate over all key-value pairs using janet_dictionary_next.
     Directly reads the data/capacity fields from the JanetTable struct,
     which avoids the void-pointer dance of janet_dictionary_view. *)
  let iter (tbl : t) ~(f : I.t -> I.t -> unit) : unit =
    let kvs = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.data in
    let cap = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.capacity |> Int32.to_int_exn in
    (* null sentinel: allocate a dummy pointer and use its null-state *)
    let null_kv = Ctypes.from_voidp T.janet_kv Ctypes.null in
    let cur = ref (F.janet_dictionary_next kvs (Int32.of_int_exn cap) null_kv) in
    while not (Ctypes.is_null !cur) do
      let kv = Ctypes.(!@(!cur)) in
      let key = Ctypes.getf kv T.janet_kv_key in
      let value = Ctypes.getf kv T.janet_kv_value in
      f key value;
      cur := F.janet_dictionary_next kvs (Int32.of_int_exn cap) !cur
    done
  ;;

  let fold (tbl : t) ~init ~(f : 'a -> I.t -> I.t -> 'a) : 'a =
    let acc = ref init in
    iter tbl ~f:(fun k v -> acc := f !acc k v);
    !acc
  ;;

  let to_pairs (tbl : t) : (I.t * I.t) list =
    fold tbl ~init:[] ~f:(fun acc k v -> (k, v) :: acc) |> List.rev
  ;;

  let to_seq (tbl : t) : (I.t * I.t) Seq.t =
    let kvs = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.data in
    let cap = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.capacity |> Int32.to_int_exn in
    let null_kv = Ctypes.from_voidp T.janet_kv Ctypes.null in
    let rec next cur () =
      let kv_ptr = F.janet_dictionary_next kvs (Int32.of_int_exn cap) cur in
      if Ctypes.is_null kv_ptr
      then Seq.Nil
      else (
        let kv = Ctypes.(!@kv_ptr) in
        Seq.Cons
          ((Ctypes.getf kv T.janet_kv_key, Ctypes.getf kv T.janet_kv_value), next kv_ptr))
    in
    next null_kv
  ;;
end
