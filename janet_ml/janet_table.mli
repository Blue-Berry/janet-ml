type t = [ `janet_table ] Ctypes.structure Ctypes_static.ptr

val sexp_of_t : 'a -> Sexplib0.Sexp.t
val create : int -> t
val get : t -> key:Janet.t -> Janet.t
val rawget : t -> key:Janet.t -> Janet.t
val remove : t -> key:Janet.t -> Janet.t
val put : t -> key:Janet.t -> value:Janet.t -> unit
val to_struct : t -> Janet_struct.t
val merge_table : t -> t -> unit
val merge_struct : t -> Janet_struct.t -> unit
val find : t -> key:Janet.t -> Janet_kv.t
val count : t -> int option
val capacity : t -> int option
val proto : t -> t option
val wrap : t -> Janet.t
val unwrap : Janet.t -> t
