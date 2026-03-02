module Janet = Janet

val init : unit -> unit
val deinit : unit -> unit
val dostring : env:Janet.Table.t -> string -> source_path:string option -> Janet.t
val dobytes : Janet.Table.t -> bytes -> string option -> Janet.t
val mcall : string -> Janet.t list -> Janet.t
