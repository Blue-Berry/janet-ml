module Janet = Janet

exception Janet_error of string

val init : unit -> unit
val deinit : unit -> unit

(** Run [f ()] with the Janet VM initialised, guaranteed to call [deinit]
    even if [f] raises. *)
val with_janet : (unit -> 'a) -> 'a

(** Like [with_janet] but also creates a core environment and passes it to [f]. *)
val with_janet_env : (Janet.Table.t -> 'a) -> 'a

(** Evaluate a Janet source string. Returns the signal and the result/error value. *)
val dostring
  :  ?source_path:string
  -> env:Janet.Table.t
  -> string
  -> Janet.Fiber.signal * Janet.t

(** Like [dostring] but raises [Janet_error] on any non-ok signal. *)
val dostring_exn : ?source_path:string -> env:Janet.Table.t -> string -> Janet.t

(** Evaluate Janet bytecode. Returns the signal and the result/error value. *)
val dobytes
  :  ?source_path:string
  -> env:Janet.Table.t
  -> bytes
  -> Janet.Fiber.signal * Janet.t

(** Like [dobytes] but raises [Janet_error] on any non-ok signal. *)
val dobytes_exn : ?source_path:string -> env:Janet.Table.t -> bytes -> Janet.t

val mcall : string -> Janet.t list -> Janet.t

(** Like [mcall] but wraps the call for a consistent exception-based interface. *)
val mcall_exn : string -> Janet.t list -> Janet.t
