module F = Janet_c.C.Functions
module T = Janet_c.C.Types
open Janet_types

type t = [ `janet_kv ] Ctypes.structure Ctypes_static.ptr

let begin_ count : t = F.janet_struct_begin (Int32.of_int count)
let put (st : t) (key : janet) (value : janet) = F.janet_struct_put st key value
let end_ (st : t) : t = F.janet_struct_end st
let get (st : t) (key : janet) : janet = F.janet_struct_get st key
let rawget (st : t) (key : janet) : janet = F.janet_struct_rawget st key
let to_table (st : t) : Janet_table.t = F.janet_struct_to_table st
let find (st : t) (key : janet) : t = F.janet_struct_find st key
let wrap (st : t) : janet = F.janet_wrap_struct st
let unwrap (j : janet) : t = F.janet_unwrap_struct j

let of_pairs (pairs : (janet * janet) list) : t =
  let st = begin_ (List.length pairs) in
  List.iter (fun (k, v) -> put st k v) pairs;
  end_ st
;;
