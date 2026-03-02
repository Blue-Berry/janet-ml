module rec Janet : sig
  type t = Type.janet
  type ptr = t Ctypes.ptr

  val of_ptr : ptr -> t
  val to_ptr : t -> ptr
  val create : unit -> t
  val create_ptr : unit -> ptr
end

and Abstract : sig
  type t = Type.abstract

  val sexp_of_t : 'a -> Core.Sexp.t
end

and Array : sig
  type t = Type.array

  val sexp_of_t : t -> Core.Sexp.t
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
end

and Buffer : sig
  type t = Type.buffer

  val sexp_of_t : t -> Core.Sexp.t
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

  val sexp_of_t : 'a -> Core.Sexp.t
end

and Function : sig
  type t = Type.function_t

  val sexp_of_t : 'a -> Sexplib0.Sexp.t

  val pcall
    :  t
    -> Janet.t list
    -> ?fiber:Janet_ml__Type.fiber option
    -> unit
    -> Janet_c.Types_generated.janet_signal * Janet.t

  val call : t -> Janet.t list -> Janet.t
end

and Struct : sig
  type t = Type.struct_t

  val sexp_of_t : t -> Core.Sexp.t
  val data_of_head : t -> Kv.t
  val head_of_data : Kv.t -> t
  val begin_ : int -> t
  val put : t -> Janet.t -> Janet.t -> unit
  val end_ : t -> t
  val get : t -> Janet.t -> Janet.t
  val rawget : t -> Janet.t -> Janet.t
  val to_table : t -> Table.t
  val find : t -> Janet.t -> Kv.t
  val length : t -> int
  val hash : t -> int32
  val capacity : t -> int
  val proto : t -> Kv.t
  val wrap : t -> Janet.t
  val unwrap : Janet.t -> t
  val of_pairs : (Janet.t * Janet.t) list -> t
end

and Table : sig
  type t = Type.table

  val sexp_of_t : t -> Core.Sexp.t
  val create : int -> t
  val get : t -> key:Janet.t -> Janet.t
  val rawget : t -> key:Janet.t -> Janet.t
  val remove : t -> key:Janet.t -> Janet.t
  val put : t -> key:Janet.t -> value:Janet.t -> unit
  val to_struct : t -> Struct.t
  val merge_table : t -> t -> unit
  val merge_struct : t -> Struct.t -> unit
  val find : t -> key:Janet.t -> Kv.t
  val count : t -> int option
  val capacity : t -> int option
  val proto : t -> t option
  val wrap : t -> Janet.t
  val unwrap : Janet.t -> t
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

  type janet_signal = Janet_c.C.Types.janet_signal

  val create : Function.t -> capacity:int -> argv:Janet.t list -> t
  val reset : t -> Function.t -> argv:Janet.t list -> t
  val status : t -> status
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val current : unit -> t
  val wrap : t -> Janet.t
  val unwrap : Janet.t -> t
  val continue : t -> Janet.t -> janet_signal * Janet.t
  val continue_signal : t -> Janet.t -> janet_signal -> janet_signal * Janet.t
  val step : t -> Janet.t -> janet_signal * Janet.t
  val set_env : t -> Table.t -> unit
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

  val sexp_of_t : 'a -> Core.Sexp.t
end

and Tuple : sig
  type t = Type.tuple

  val sexp_of_t : t -> Core.Sexp.t
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
end

and Vm : sig
  type t = Type.vm

  val local_vm : unit -> t
  val create : unit -> t
  val free : t -> unit
  val save : unit -> t
  val load : t -> unit
end

and Env : sig
  type t = Table.t

  val core_env : replacements:t option -> t

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
  val marshal
    :  ?unsafe:bool
    -> ?no_cycles:bool
    -> ?max_size:int
    -> env:Env.t option
    -> Janet.t
    -> string

  val marshal_symbol
    :  ?unsafe:bool
    -> ?no_cycles:bool
    -> ?max_size:int
    -> env:Env.t
    -> string
    -> string

  val unmarshal
    :  ?unsafe:bool
    -> ?no_cycles:bool
    -> ?env:Env.t option
    -> string
    -> Janet.t
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
    | Array of t list
    | Tuple of t list
    | Table of Table.t
    | Struct of Struct.t
    | Buffer of bytes
    | Function of Function.t
    | CFunction of Cfunction.t
    | Abstract of Abstract.t
    | Pointer of Pointer.t

  val sexp_of_t : t -> Sexplib0.Sexp.t

  type janet_type

  val check_type : janet -> Janet_c.Function_description.Types.janet_type
  val of_janet : Janet.t -> t
end

type t = Janet.t
type ptr = t Ctypes.ptr

val of_ptr : ptr -> t
val to_ptr : t -> ptr
val create : unit -> t
val create_ptr : unit -> ptr
val sexp_of_t : Janet.t -> Sexplib0.Sexp.t
