module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type t = [ `janet_table ] Ctypes.structure Ctypes_static.ptr

let create capacity : t = F.janet_table (Int32.of_int capacity)

let get (tbl : t) (key : Janet_types.janet) : Janet_types.janet =
  F.janet_table_get tbl key
;;

let rawget (tbl : t) (key : Janet_types.janet) : Janet_types.janet =
  F.janet_table_rawget tbl key
;;

let remove (tbl : t) (key : Janet_types.janet) : Janet_types.janet =
  F.janet_table_remove tbl key
;;

let put (tbl : t) (key : Janet_types.janet) (value : Janet_types.janet) =
  F.janet_table_put tbl key value
;;

let to_struct (tbl : t) : [ `janet_kv ] Ctypes.structure Ctypes_static.ptr =
  F.janet_table_to_struct tbl
;;

let merge_table (tbl : t) (other : t) = F.janet_table_merge_table tbl other

let merge_struct (tbl : t) (other : [ `janet_kv ] Ctypes.structure Ctypes_static.ptr) =
  F.janet_table_merge_struct tbl other
;;

let find (tbl : t) (key : Janet_types.janet) = F.janet_table_find tbl key
let count (tbl : t) = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.count |> Int32.to_int
let capacity (tbl : t) = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.capacity |> Int32.to_int
let proto (tbl : t) : t option = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.proto
let wrap (tbl : t) : Janet_types.janet = F.janet_wrap_table tbl
let unwrap (j : Janet_types.janet) : t = F.janet_unwrap_table j
