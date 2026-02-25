module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type t = [ `janet_tuple_head ] Ctypes.structure Ctypes_static.ptr

(* Convert between head pointer and data pointer (the flexible array member).
   sizeof(JanetTupleHead) == offsetof(JanetTupleHead, data) *)
let data_of_head (h : t) : [ `janet ] Ctypes.structure Ctypes_static.ptr =
  let offset = Ctypes.sizeof T.Janet_Tuple.head in
  let p = Ctypes.from_voidp Ctypes.char (Ctypes.to_voidp h) in
  Ctypes.from_voidp T.janet (Ctypes.to_voidp Ctypes.(p +@ offset))
;;

let head_of_data (d : [ `janet ] Ctypes.structure Ctypes_static.ptr) : t =
  let offset = Ctypes.sizeof T.Janet_Tuple.head in
  let p = Ctypes.from_voidp Ctypes.char (Ctypes.to_voidp d) in
  Ctypes.from_voidp T.Janet_Tuple.head (Ctypes.to_voidp Ctypes.(p +@ -offset))
;;

(* Builder API *)
let begin_ length : t = head_of_data (F.janet_tuple_begin (Int32.of_int length))

let set (builder : t) (i : int) (value : Janet.t) =
  let d = data_of_head builder in
  Ctypes.(d +@ i <-@ value)
;;

let end_ (builder : t) : t = head_of_data (F.janet_tuple_end (data_of_head builder))

let of_list (values : Janet.t list) : t =
  let n = List.length values in
  let c_arr = Ctypes.CArray.of_list T.janet values in
  head_of_data (F.janet_tuple_n (Ctypes.CArray.start c_arr) (Int32.of_int n))
;;

let of_array (values : Janet.t array) : t = of_list (Array.to_list values)

(* Field accessors via the head struct *)
let length (tup : t) : int =
  Ctypes.getf Ctypes.(!@tup) T.Janet_Tuple.length |> Int32.to_int
;;

let hash (tup : t) : int32 = Ctypes.getf Ctypes.(!@tup) T.Janet_Tuple.hash
let sm_line (tup : t) : int32 = Ctypes.getf Ctypes.(!@tup) T.Janet_Tuple.sm_line
let sm_column (tup : t) : int32 = Ctypes.getf Ctypes.(!@tup) T.Janet_Tuple.sm_column

(* Element access via the data pointer *)
let get (tup : t) (i : int) : Janet.t =
  let d = data_of_head tup in
  Ctypes.(!@(d +@ i))
;;

let to_list (tup : t) : Janet.t list =
  let n = length tup in
  Ctypes.CArray.from_ptr (data_of_head tup) n |> Ctypes.CArray.to_list
;;

let to_array (tup : t) : Janet.t array = to_list tup |> Array.of_list

(* Wrap/unwrap convert between head pointer and Janet value *)
let wrap (tup : t) : Janet.t = F.janet_wrap_tuple (data_of_head tup)
let unwrap (j : Janet.t) : t = head_of_data (F.janet_unwrap_tuple j)
