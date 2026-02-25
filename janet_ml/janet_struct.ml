module F = Janet_c.C.Functions
module T = Janet_c.C.Types
open Janet_types

type t = [ `janet_struct_head ] Ctypes.structure Ctypes_static.ptr

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
let begin_ count : t = head_of_data (F.janet_struct_begin (Int32.of_int count))

let put (st : t) (key : janet) (value : janet) =
  F.janet_struct_put (data_of_head st) key value
;;

let end_ (st : t) : t = head_of_data (F.janet_struct_end (data_of_head st))

(* Lookup *)
let get (st : t) (key : janet) : janet = F.janet_struct_get (data_of_head st) key
let rawget (st : t) (key : janet) : janet = F.janet_struct_rawget (data_of_head st) key

(* Conversion *)
let to_table (st : t) : [ `janet_table ] Ctypes.structure Ctypes_static.ptr =
  F.janet_struct_to_table (data_of_head st)
;;

let find (st : t) (key : janet) : [ `janet_kv ] Ctypes.structure Ctypes_static.ptr =
  F.janet_struct_find (data_of_head st) key
;;

(* Field accessors via the head struct *)
let length (st : t) : int =
  Ctypes.getf Ctypes.(!@st) T.Janet_Struct.length |> Int32.to_int
;;

let hash (st : t) : int32 = Ctypes.getf Ctypes.(!@st) T.Janet_Struct.hash

let capacity (st : t) : int =
  Ctypes.getf Ctypes.(!@st) T.Janet_Struct.capacity |> Int32.to_int
;;

let proto (st : t) : [ `janet_kv ] Ctypes.structure Ctypes_static.ptr =
  Ctypes.getf Ctypes.(!@st) T.Janet_Struct.proto
;;

(* Wrap/unwrap convert between head pointer and Janet value *)
let wrap (st : t) : janet = F.janet_wrap_struct (data_of_head st)
let unwrap (j : janet) : t = head_of_data (F.janet_unwrap_struct j)

let of_pairs (pairs : (janet * janet) list) : t =
  let st = begin_ (List.length pairs) in
  List.iter (fun (k, v) -> put st k v) pairs;
  end_ st
;;
