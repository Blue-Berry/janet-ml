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

let to_struct (tbl : t) : Janet_types.janet_struct =
  let data = F.janet_table_to_struct tbl in
  let offset = Ctypes.sizeof T.Janet_Struct.head in
  let p = Ctypes.from_voidp Ctypes.char (Ctypes.to_voidp data) in
  Ctypes.from_voidp T.Janet_Struct.head (Ctypes.to_voidp Ctypes.(p +@ -offset))
;;

let merge_table (tbl : t) (other : t) = F.janet_table_merge_table tbl other

let merge_struct (tbl : t) (other : Janet_types.janet_struct) =
  let offset = Ctypes.sizeof T.Janet_Struct.head in
  let p = Ctypes.from_voidp Ctypes.char (Ctypes.to_voidp other) in
  let data = Ctypes.from_voidp T.janet_kv (Ctypes.to_voidp Ctypes.(p +@ offset)) in
  F.janet_table_merge_struct tbl data
;;

let find (tbl : t) (key : Janet_types.janet) = F.janet_table_find tbl key
let count (tbl : t) = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.count |> Int32.to_int
let capacity (tbl : t) = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.capacity |> Int32.to_int
let proto (tbl : t) : t option = Ctypes.getf Ctypes.(!@tbl) T.Janet_Table.proto
let wrap (tbl : t) : Janet_types.janet = F.janet_wrap_table tbl
let unwrap (j : Janet_types.janet) : t = F.janet_unwrap_table j
