module rec Janet : sig
  type t = Type.janet
  type ptr = t Ctypes.ptr

  val of_ptr : ptr -> t
  val to_ptr : t -> ptr
  val create : unit -> t
  val create_ptr : unit -> ptr

  (** Root [v] against the Janet GC for the duration of [f v], unrooting it
      afterwards even if [f] raises. Use this to keep a [Janet.t] alive across
      any allocating Janet operation. *)
  val with_root : t -> f:(t -> 'a) -> 'a

  (* -- Convenience constructors -- *)

  val nil : t
  val of_float : float -> t
  val of_int : int -> t
  val of_bool : bool -> t
  val of_string : string -> t
  val of_keyword : string -> t
  val of_symbol : string -> t

  (* -- Predicates / inspection -- *)

  type janet_type = Janet_c.C.Types.janet_type

  val is_nil : t -> bool
  val typeof : t -> janet_type
end

and Abstract : sig
  type t = Type.abstract

  val sexp_of_t : 'a -> Core.Sexp.t
end

and Array : sig
  type t = Type.array

  val sexp_of_t : t -> Core.Sexp.t
  val t_of_sexp : Core.Sexp.t -> t
  val create : int -> t
  val push : t -> Janet.t -> unit
  val pop : t -> Janet.t
  val peek : t -> Janet.t
  val count : t -> int
  val capacity : t -> int
  val get : t -> int -> Janet.t
  val set : t -> int -> Janet.t -> unit
  val to_list : t -> Janet.t list
  val to_array : t -> Janet.t array
  val of_list : Janet.t list -> t
  val of_array : Janet.t array -> t
  val wrap : t -> Janet.t
  val unwrap : Janet.t -> t

  (** Pre-allocate storage: ensure at least [capacity] slots, growing by [growth]. *)
  val ensure : t -> capacity:int -> growth:int -> unit

  (** Set the logical element count, zero-padding or truncating as needed. *)
  val set_count : t -> int -> unit

  (** Create a Janet array from an OCaml array using a single C allocation. *)
  val of_janet_array : Janet.t array -> t

  (** Lazy sequence over the array elements. *)
  val to_seq : t -> Janet.t Seq.t
end

and Buffer : sig
  type t = Type.buffer

  val sexp_of_t : t -> Core.Sexp.t
  val t_of_sexp : Core.Sexp.t -> t
  val create : int -> t
  val deinit : t -> unit
  val ensure : t -> capacity:int -> growth:int -> unit
  val set_count : t -> int -> unit
  val extra : t -> int -> unit
  val push_bytes : t -> bytes -> unit
  val push_string : t -> string -> unit
  val push_u8 : t -> int -> unit
  val push_u16 : t -> int -> unit
  val push_u32 : t -> int -> unit
  val push_u64 : t -> int -> unit
  val count : t -> int
  val capacity : t -> int
  val contents : t -> bytes
  val to_string : t -> string
  val wrap : t -> Janet.t
  val unwrap : Janet.t -> t
end

and Cfunction : sig
  type t = Type.cfunction

  val sexp_of_t : t -> Core.Sexp.t
end

and Cfun : sig
  type handle

  val free : handle -> unit
  val register : env:Table.t -> string -> (Janet.t array -> Janet.t) -> handle

  val register_raw
    :  env:Table.t
    -> string
    -> (int32 -> Janet.t Ctypes.ptr -> Janet.t)
    -> handle
end

and Function : sig
  type t = Type.function_t

  val sexp_of_t : t -> Sexplib0.Sexp.t

  val pcall
    :  t
    -> Janet.t list
    -> ?fiber:Janet_ml__Type.fiber option
    -> unit
    -> Fiber.signal * Janet.t

  (** Call [f] with [pcall] and raise [Janet_errors.Janet_error] on any
      non-ok signal. Prefer this over [call] for production code. *)
  val call_exn : t -> Janet.t list -> Janet.t

  (** Direct call via [janet_call]. A Janet panic terminates the process;
      prefer [call_exn] or [pcall] for safety. *)
  val call : t -> Janet.t list -> Janet.t

  val to_janet : t -> Janet.t
end

and Struct : sig
  type t = Type.struct_t

  val sexp_of_t : t -> Core.Sexp.t
  val put : t -> Janet.t -> Janet.t -> unit
  val get : t -> key:Janet.t -> Janet.t
  val rawget : t -> key:Janet.t -> Janet.t
  val to_table : t -> Table.t
  val find : t -> Janet.t -> Kv.t
  val length : t -> int
  val hash : t -> int32
  val capacity : t -> int
  val proto : t -> Kv.t
  val wrap : t -> Janet.t
  val unwrap : Janet.t -> t
  val of_pairs : (Janet.t * Janet.t) list -> t
  val to_janet : t -> Janet.t

  (** Iterate over all key-value pairs in the struct. *)
  val iter : t -> f:(Janet.t -> Janet.t -> unit) -> unit

  (** Fold over all key-value pairs in the struct. *)
  val fold : t -> init:'a -> f:('a -> Janet.t -> Janet.t -> 'a) -> 'a

  (** Return all key-value pairs as a list. *)
  val to_pairs : t -> (Janet.t * Janet.t) list

  (** Lazy sequence over key-value pairs. *)
  val to_seq : t -> (Janet.t * Janet.t) Seq.t
end

and Table : sig
  type t = Type.table

  val sexp_of_t : t -> Core.Sexp.t
  val create : int -> t
  val of_pairs : (Janet.t * Janet.t) list -> t
  val get : t -> key:Janet.t -> Janet.t
  val rawget : t -> key:Janet.t -> Janet.t
  val remove : t -> key:Janet.t -> Janet.t
  val put : t -> key:Janet.t -> value:Janet.t -> unit
  val to_struct : t -> Struct.t
  val merge_table : t -> t -> unit
  val merge_struct : t -> Struct.t -> unit
  val find : t -> key:Janet.t -> Kv.t
  val count : t -> int
  val capacity : t -> int
  val proto : t -> t option
  val wrap : t -> Janet.t
  val unwrap : Janet.t -> t
  val to_janet : t -> Janet.t

  (** Iterate over all key-value pairs in the table. *)
  val iter : t -> f:(Janet.t -> Janet.t -> unit) -> unit

  (** Fold over all key-value pairs in the table. *)
  val fold : t -> init:'a -> f:('a -> Janet.t -> Janet.t -> 'a) -> 'a

  (** Return all key-value pairs as a list. *)
  val to_pairs : t -> (Janet.t * Janet.t) list

  (** Lazy sequence over key-value pairs. *)
  val to_seq : t -> (Janet.t * Janet.t) Seq.t
end

and Fiber : sig
  type t = Type.fiber

  val stacktrace : t -> Janet.t -> unit

  type status =
    | Dead
    | Error
    | Debug
    | Pending
    | New
    | Alive
    | User of int

  val sexp_of_status : status -> Sexplib0.Sexp.t

  type signal =
    | Signal_ok
    | Signal_error
    | Signal_debug
    | Signal_yield
    | Signal_user0
    | Signal_user1
    | Signal_user2
    | Signal_user3
    | Signal_user4
    | Signal_user5
    | Signal_user6
    | Signal_user7
    | Signal_user8
    | Signal_user9

  val signal_to_string : signal -> string
  val create : Function.t -> capacity:int -> argv:Janet.t list -> t
  val reset : t -> Function.t -> argv:Janet.t list -> t
  val status : t -> status
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val current : unit -> t
  val wrap : t -> Janet.t
  val unwrap : Janet.t -> t
  val continue : t -> Janet.t -> signal * Janet.t
  val continue_signal : t -> Janet.t -> signal -> signal * Janet.t
  val step : t -> Janet.t -> signal * Janet.t
  val set_env : t -> Table.t -> unit
  val to_janet : t -> Janet.t

  (** Inject [msg] as an error into [fiber] by resuming it with
      [JANET_SIGNAL_ERROR] via [janet_continue_signal]. Safe for any fiber. *)
  val cancel : t -> Janet.t -> unit

  (** Raw [janet_cancel]. Only safe for root/task fibers used with the Janet
      event loop. Will call [exit] if the fiber is not a root fiber. *)
  val cancel_ev : t -> Janet.t -> unit

  (** Return true if the fiber can be resumed (status New or Pending). *)
  val can_resume : t -> bool

  (** Read the last value produced by the fiber (e.g. its return value once Dead). *)
  val last_value : t -> Janet.t
end

and Compile_result : sig
  type t = Type.compile_result

  type status =
    | Ok
    | Error

  val funcdef : t -> Type.funcdef
  val error : t -> Unsigned.UInt8.t Ctypes_static.ptr
  val error_string : t -> string
  val macrofiber : t -> Fiber.t
  val error_mapping_line : t -> int
  val error_mapping_column : t -> int
  val status : t -> status
end

and Compile : sig
  type funcdef

  type error =
    { message : string
    ; line : int
    ; column : int
    }

  type result = (funcdef, error) Stdlib.result

  val extract_result : Compile_result.t -> result
  val compile : Janet.t -> Table.t -> string -> result
  val compile_lint : Janet.t -> Table.t -> string -> Array.t -> result
  val to_function : funcdef -> Function.t
end

and Kv : sig
  type t = Type.kv

  val sexp_of_t : t -> Core.Sexp.t
  val key : t -> Janet.t
  val value : t -> Janet.t
  val is_null : t -> bool
  val to_pair : t -> Janet.t * Janet.t
end

and Parser : sig
  type t = Type.parser

  type status =
    | Root
    | Error
    | Pending
    | Dead

  val create : unit -> t
  val to_janet : t -> Janet.t
  val consume : t -> char -> unit
  val consume_string : t -> string -> unit
  val eof : t -> unit
  val has_more : t -> bool
  val produce : t -> Janet.t
  val status : t -> status
  val error : t -> string option
  val flush : t -> unit
  val line : t -> int
  val column : t -> int
end

and Pointer : sig
  type t = Type.pointer

  val sexp_of_t : t -> Core.Sexp.t
  val to_janet : t -> Janet.t
  val to_janet : t -> Janet.t
end

and Tuple : sig
  type t = Type.tuple

  val sexp_of_t : t -> Core.Sexp.t
  val t_of_sexp : Core.Sexp.t -> t
  val set : t -> int -> Janet.t -> unit
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
  val to_janet : t -> Janet.t

  (** Lazy sequence over the tuple elements. *)
  val to_seq : t -> Janet.t Seq.t
end

and Vm : sig
  type t = Type.vm

  val init : unit -> unit
  val deinit : unit -> unit
  val with_vm : (unit -> 'a) -> 'a
  val local_vm : unit -> t
  val create : unit -> t
  val free : t -> unit
  val save : unit -> t
  val load : t -> unit
end

and Ev : sig
  (** Run the event loop until all scheduled fibers complete. *)
  val run : unit -> unit

  (** Return whether the event loop has no more work to do. *)
  val is_done : unit -> bool

  (** Execute one turn of the event loop. Returns [Some fiber] if a fiber
      was interrupted via {!interrupt}, or [None] otherwise.
      Use with {!is_done} for a custom event loop with periodic work. *)
  val step : unit -> Fiber.t option

  (** Request an interrupt on the given VM. Can be called from another thread
      or a signal handler. Causes the next {!step} call to return the
      interrupted fiber. *)
  val interrupt : Vm.t -> unit

  (** Schedule a fiber to run on the event loop with [value] as resume value. *)
  val schedule : Fiber.t -> Janet.t -> unit

  (** Schedule a fiber with a specific signal. *)
  val schedule_signal : Fiber.t -> Janet.t -> Fiber.signal -> unit

  (** Schedule a fiber soon (deferred scheduling). *)
  val schedule_soon : Fiber.t -> Janet.t -> Fiber.signal -> unit

  (** Run [fiber] as the entrypoint of the event loop. Returns 0 on success,
      non-zero on error. This is a convenience that schedules the fiber and
      runs the loop. *)
  val loop_fiber : Fiber.t -> int
end

and Env : sig
  type t = Table.t

  val core_env : ?replacements:Table.t -> unit -> t

  (** Bind [name] to [value] as an immutable def in [env]. *)
  val def : t -> ?doc:string -> string -> Janet.t -> unit

  (** Bind [name] to [value] as a mutable var in [env]. *)
  val var : t -> ?doc:string -> string -> Janet.t -> unit

  val to_janet : t -> Janet.t
  val sexp_of_t : t -> Core.Sexp.t

  module Binding : sig
    type env = t

    type t =
      | None
      | Def of Janet.t
      | Var of Janet.t
      | Macro of Janet.t
      | Dynamic_def of Janet.t
      | Dynamic_macro of Janet.t

    val lookup : env:env -> string -> t
    val to_janet : t -> Janet.t
  end
end

and Marshal : sig
  (** Marshal a Janet value to a binary image string.
      [rreg] is the reverse registry (value→symbol, from [make-image-dict]). *)
  val marshal
    :  ?unsafe:bool
    -> ?no_cycles:bool
    -> ?max_size:int
    -> ?rreg:Env.t
    -> Janet.t
    -> string

  val marshal_symbol
    :  ?unsafe:bool
    -> ?no_cycles:bool
    -> ?max_size:int
    -> env:Env.t
    -> string
    -> string

  (** Return the [make-image-dict] table from [env] (value→symbol mapping),
      suitable for use as the [~rreg] parameter to {!marshal}. *)
  val make_image_dict : Env.t -> Env.t

  (** Return the [load-image-dict] table from [env] (symbol→value mapping),
      suitable for use as the [~reg] parameter to {!unmarshal}. *)
  val load_image_dict : Env.t -> Env.t

  (** Unmarshal a binary image string back to a Janet value.
      [reg] is the registry (symbol→value, from [load-image-dict]).
      Defaults to [load_image_dict] from the core env. *)
  val unmarshal : ?unsafe:bool -> ?no_cycles:bool -> ?reg:Env.t -> string -> Janet.t
end

and Unwrapped : sig
  type janet = Janet.t

  type t =
    | Number of float
    | Nil
    | Boolean of bool
    | Fiber of Fiber.t
    | String of string
    | Symbol of string
    | Keyword of string
    | Array of Array.t
    | Tuple of Tuple.t
    | Table of Table.t
    | Struct of Struct.t
    | Buffer of bytes
    | Function of Function.t
    | CFunction of Cfunction.t
    | Int of int64
    | UInt of Unsigned.uint64
    | Abstract of Abstract.t
    | Pointer of Pointer.t

  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t

  type janet_type

  val check_type : janet -> Janet_c.Function_description.Types.janet_type
  val to_janet : t -> Janet.t
  val of_janet : Janet.t -> t
end

type t = Janet.t
type ptr = t Ctypes.ptr

val of_ptr : ptr -> t
val to_ptr : t -> ptr
val create : unit -> t
val create_ptr : unit -> ptr
val with_root : t -> f:(t -> 'a) -> 'a
val gc_root : t -> unit
val gc_unroot : t -> unit

(* -- Convenience constructors -- *)

val nil : t
val of_float : float -> t
val of_int : int -> t
val of_bool : bool -> t
val of_string : string -> t
val of_keyword : string -> t
val of_symbol : string -> t

(* -- Predicates / inspection -- *)

type janet_type = Janet_c.C.Types.janet_type

val is_nil : t -> bool
val typeof : t -> janet_type

(** Wrap an OCaml [int64] as a Janet s64 abstract value. *)
val wrap_s64 : int64 -> t

(** Wrap a [uint64] as a Janet u64 abstract value. *)
val wrap_u64 : Unsigned.uint64 -> t

(** Unwrap a Janet s64 value. Call only when the value is known to be s64. *)
val unwrap_s64 : t -> int64

(** Unwrap a Janet u64 value. Call only when the value is known to be u64. *)
val unwrap_u64 : t -> Unsigned.uint64

(** Human-readable description of a Janet value (like Janet's [describe]). *)
val to_string : t -> string

(** String conversion of a Janet value (like Janet's [string]).
    For string values returns raw content; for others equivalent to [to_string]. *)
val to_string_value : t -> string

(** Pretty-print a Janet value. [depth] controls nesting depth (default 4). *)
val pretty : ?depth:int -> t -> string

(* -- Generic operations -- *)

val equal : t -> t -> bool
val length : t -> int
val get : t -> t -> t
val put : t -> key:t -> value:t -> unit
val truthy : t -> bool
val gc_collect : unit -> unit

(* -- Dynamic bindings -- *)

val dyn : string -> t
val setdyn : string -> t -> unit
val sexp_of_t : Janet.t -> Sexplib0.Sexp.t
val t_of_sexp : Sexplib0.Sexp.t -> Janet.t

exception Janet_error of string

val check_signal : Fiber.signal -> t -> unit
val dostring : ?source_path:string -> env:Table.t -> string -> Fiber.signal * t
val dostring_exn : ?source_path:string -> env:Table.t -> string -> t
val dobytes : ?source_path:string -> env:Table.t -> bytes -> Fiber.signal * t
val dobytes_exn : ?source_path:string -> env:Table.t -> bytes -> t
val mcall : string -> t list -> t
val mcall_exn : string -> t list -> t
val with_janet_env : (Env.t -> 'a) -> 'a

(* -- Conversions from Janet values -- *)

val to_keyword : t -> string option
val to_keyword_exn : t -> string
val to_number : t -> float option
val to_number_exn : t -> float
val to_function : t -> Function.t option
val to_function_exn : t -> Function.t
val to_bool : t -> bool option
val to_bool_exn : t -> bool
val to_janet_string : t -> string option
val to_janet_string_exn : t -> string
val to_symbol : t -> string option
val to_symbol_exn : t -> string
val to_array : t -> Array.t option
val to_array_exn : t -> Array.t
val to_tuple : t -> Tuple.t option
val to_tuple_exn : t -> Tuple.t
val to_table : t -> Table.t option
val to_table_exn : t -> Table.t
val to_struct : t -> Struct.t option
val to_struct_exn : t -> Struct.t
val to_buffer : t -> bytes option
val to_buffer_exn : t -> bytes
val to_fiber : t -> Fiber.t option
val to_fiber_exn : t -> Fiber.t
val to_cfunction : t -> Cfunction.t option
val to_cfunction_exn : t -> Cfunction.t
val to_int64 : t -> int64 option
val to_int64_exn : t -> int64
val to_uint64 : t -> Unsigned.uint64 option
val to_uint64_exn : t -> Unsigned.uint64
