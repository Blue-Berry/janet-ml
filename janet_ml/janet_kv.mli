type t = [ `janet_kv ] Ctypes.structure Ctypes_static.ptr

val sexp_of_t : 'a -> Sexplib0.Sexp.t
val key : t -> Janet.t
val value : t -> Janet.t
val is_null : t -> bool
val to_pair : t -> Janet.t * Janet.t
