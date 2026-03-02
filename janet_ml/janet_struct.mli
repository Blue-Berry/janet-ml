type t = [ `janet_struct_head ] Ctypes.structure Ctypes_static.ptr

val sexp_of_t : 'a -> Sexplib0.Sexp.t
val data_of_head : t -> Janet_kv.t
val head_of_data : Janet_kv.t -> t
val put : t -> Janet.t -> Janet.t -> unit
val get : t -> Janet.t -> Janet.t
val rawget : t -> Janet.t -> Janet.t
val to_table : t -> [ `janet_table ] Ctypes.structure Ctypes_static.ptr
val find : t -> Janet.t -> Janet_kv.t
val length : t -> int
val hash : t -> int32
val capacity : t -> int
val proto : t -> Janet_kv.t
val wrap : t -> Janet.t
val unwrap : Janet.t -> t
val of_pairs : (Janet.t * Janet.t) list -> t
