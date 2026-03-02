open! Core
module T = Janet_c.C.Types

type t = Type.kv

let sexp_of_t _ = Sexp.of_string "janet_kv"
let key (kv : t) : Janet.t = Ctypes.getf Ctypes.(!@kv) T.janet_kv_key
let value (kv : t) : Janet.t = Ctypes.getf Ctypes.(!@kv) T.janet_kv_value
let is_null (kv : t) : bool = Ctypes.is_null kv
let to_pair (kv : t) : Janet.t * Janet.t = key kv, value kv
