(** Higher-level callback registration for Janet C-functions.

    [Cfun.register] provides a simpler interface than [Janet_ml.register_cfun]:
    the callback receives a plain [Janet.t array] instead of raw C argc/argv. *)

(** Opaque handle keeping the libffi closure alive. Retain this for as long as
    Janet may call back into the registered function. *)
type handle

(** Release the libffi closure. After this, Janet must no longer call the function. *)
val free : handle -> unit

(** Register an OCaml function as a Janet C-function named [name] in [env].
    The callback receives the arguments as an OCaml array and returns a [Janet.t]. *)
val register : env:Janet.Table.t -> string -> (Janet.t array -> Janet.t) -> handle

val cfun_fn_type : (int32 -> Janet.t Ctypes.ptr -> Janet.t) Ctypes.fn

module JanetCfun : Foreign.Funptr with type fn = int32 -> Janet.t Ctypes.ptr -> Janet.t

(** Like [register] but takes a raw C-level callback [(argc, argv) -> Janet.t]
    instead of an OCaml array wrapper. *)
val register_raw
  :  env:Janet.Table.t
  -> string
  -> (int32 -> Janet.t Ctypes.ptr -> Janet.t)
  -> JanetCfun.t
