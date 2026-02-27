type t

val of_list : Janet.t list -> t
val of_array : Janet.t array -> t
val length : t -> int
val hash : t -> int32
val sm_line : t -> int32
val sm_column : t -> int32
val get : t -> int -> Janet.t
val to_list : t -> Janet.t list
val to_array : t -> Janet.t array
val wrap : t -> Janet.t
val unwrap : Janet.t -> t
