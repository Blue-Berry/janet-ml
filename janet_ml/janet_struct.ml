module Make (I : Janet_sig.S) = struct
  open! Core
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types

  type t = Type.struct_t

  let sexp_of_t _ = Sexp.of_string "janet_struct"

  (* Convert between head pointer and data pointer (the flexible array member).
     sizeof(JanetStructHead) == offsetof(JanetStructHead, data) *)
  let data_of_head (h : t) : [ `janet_kv ] Ctypes.structure Ctypes_static.ptr =
    let offset = Ctypes.sizeof T.Janet_Struct.head in
    let p = Ctypes.from_voidp Ctypes.char (Ctypes.to_voidp h) in
    Ctypes.from_voidp T.janet_kv (Ctypes.to_voidp Ctypes.(p +@ offset))
  ;;

  let head_of_data (d : [ `janet_kv ] Ctypes.structure Ctypes_static.ptr) : t =
    let offset = Ctypes.sizeof T.Janet_Struct.head in
    let p = Ctypes.from_voidp Ctypes.char (Ctypes.to_voidp d) in
    Ctypes.from_voidp T.Janet_Struct.head (Ctypes.to_voidp Ctypes.(p +@ -offset))
  ;;

  (* Builder API *)
  let begin_ count : t = head_of_data (F.janet_struct_begin (Int32.of_int_exn count))

  let put (st : t) (key : I.t) (value : I.t) =
    F.janet_struct_put (data_of_head st) key value
  ;;

  let end_ (st : t) : t = head_of_data (F.janet_struct_end (data_of_head st))

  (* Lookup *)
  let get (st : t) ~(key : I.t) : I.t = F.janet_struct_get (data_of_head st) key
  let rawget (st : t) ~(key : I.t) : I.t = F.janet_struct_rawget (data_of_head st) key

  (* Conversion *)
  let to_table (st : t) : [ `janet_table ] Ctypes.structure Ctypes_static.ptr =
    F.janet_struct_to_table (data_of_head st)
  ;;

  let find (st : t) (key : I.t) : [ `janet_kv ] Ctypes.structure Ctypes_static.ptr =
    F.janet_struct_find (data_of_head st) key
  ;;

  (* Field accessors via the head struct *)
  let length (st : t) : int =
    Ctypes.getf Ctypes.(!@st) T.Janet_Struct.length |> Int32.to_int_exn
  ;;

  let hash (st : t) : int32 = Ctypes.getf Ctypes.(!@st) T.Janet_Struct.hash

  let capacity (st : t) : int =
    Ctypes.getf Ctypes.(!@st) T.Janet_Struct.capacity |> Int32.to_int_exn
  ;;

  let proto (st : t) : [ `janet_kv ] Ctypes.structure Ctypes_static.ptr =
    Ctypes.getf Ctypes.(!@st) T.Janet_Struct.proto
  ;;

  (* Wrap/unwrap convert between head pointer and Janet value *)
  let wrap (st : t) : I.t = F.janet_wrap_struct (data_of_head st)
  let to_janet = wrap

  let sexp_of_t t =
    Sexp.List [ Sexp.Atom "Struct"; to_janet t |> I.to_string |> Sexp.of_string ]
  ;;

  let unwrap (j : I.t) : t = head_of_data (F.janet_unwrap_struct j)

  let of_pairs (pairs : (I.t * I.t) list) : t =
    let st = begin_ (List.length pairs) in
    List.iter ~f:(fun (k, v) -> put st k v) pairs;
    end_ st
  ;;

  (* Iterate over all occupied key-value pairs using janet_dictionary_next. *)
  let iter (st : t) ~(f : I.t -> I.t -> unit) : unit =
    let kvs = data_of_head st in
    let cap = capacity st in
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

  let fold (st : t) ~init ~(f : 'a -> I.t -> I.t -> 'a) : 'a =
    let acc = ref init in
    iter st ~f:(fun k v -> acc := f !acc k v);
    !acc
  ;;

  let to_pairs (st : t) : (I.t * I.t) list =
    fold st ~init:[] ~f:(fun acc k v -> (k, v) :: acc) |> List.rev
  ;;

  let to_seq (st : t) : (I.t * I.t) Seq.t =
    let kvs = data_of_head st in
    let cap = capacity st in
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
