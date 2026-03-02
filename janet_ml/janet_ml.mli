module Janet = Janet

exception Janet_error of string

val init : unit -> unit
val deinit : unit -> unit

(** Run [f ()] with the Janet VM initialised, guaranteed to call [deinit]
    even if [f] raises. *)
val with_janet : (unit -> 'a) -> 'a

(** Like [with_janet] but also creates a core environment and passes it to [f]. *)
val with_janet_env : (Janet.Table.t -> 'a) -> 'a

type signal = Janet_c.C.Types.janet_signal

(** Evaluate a Janet source string. Returns the signal and the result/error value. *)
val dostring
  :  env:Janet.Table.t
  -> string
  -> source_path:string option
  -> signal * Janet.t

(** Like [dostring] but raises [Janet_error] on any non-ok signal. *)
val dostring_exn : env:Janet.Table.t -> string -> source_path:string option -> Janet.t

(** Evaluate Janet bytecode. Returns the signal and the result/error value. *)
val dobytes : Janet.Table.t -> bytes -> string option -> signal * Janet.t

(** Like [dobytes] but raises [Janet_error] on any non-ok signal. *)
val dobytes_exn : Janet.Table.t -> bytes -> string option -> Janet.t

val mcall : string -> Janet.t list -> Janet.t

module JanetCfun : Foreign.Funptr with type fn = int32 -> Janet.t Ctypes.ptr -> Janet.t

(** Register an OCaml closure as a Janet C-function named [name] in [env].
    The callback receives [(argc : int32) (argv : Janet.t ptr)] and must return
    a [Janet.t]. Use [Ctypes.CArray.from_ptr argv (Int32.to_int_exn argc)] to
    access arguments safely.

    The returned [JanetCfun.t] keeps the libffi closure alive. Retain it for
    as long as Janet may call back into OCaml. Call [JanetCfun.free] to release
    it when done. *)
val register_cfun
  :  env:Janet.Table.t
  -> string
  -> (int32 -> Janet.t Ctypes.ptr -> Janet.t)
  -> JanetCfun.t
