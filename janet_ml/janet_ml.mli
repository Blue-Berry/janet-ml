module Janet = Janet

val janet_init : unit -> unit
val janet_deinit : unit -> unit
val janet_dostring : env:Janet.Table.t -> string -> source_path:string option -> Janet.t
val janet_dobytes : Janet.Table.t -> bytes -> string option -> Janet.t
val mcall : string -> Janet.t list -> Janet.t
