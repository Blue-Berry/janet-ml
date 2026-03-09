open! Core

module Janet = struct
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types

  type t = Type.janet
  type ptr = t Ctypes.ptr

  let of_ptr : ptr -> t = Ctypes.( !@ )
  let to_ptr : t -> ptr = Ctypes.addr
  let create () : t = F.janet_wrap_nil ()
  let create_ptr () : ptr = Ctypes.addr (create ())

  (** Root [v] against the Janet GC for the duration of [f v], then unroot it.
      Use this whenever a [Janet.t] value must survive across an allocating
      operation (e.g. another [dostring], fiber continuation, etc.). *)
  let with_root (v : t) ~(f : t -> 'a) : 'a =
    F.janet_gcroot v;
    Fun.protect ~finally:(fun () -> ignore (F.janet_gcunroot v)) (fun () -> f v)
  ;;

  let gc_root (v : t) : unit = F.janet_gcroot v
  let gc_unroot (v : t) : unit = F.janet_gcunroot v |> ignore

  (* -- Convenience constructors -- *)

  let nil : t = F.janet_wrap_nil ()
  let of_float (x : float) : t = F.janet_wrap_number x
  let of_int (x : int) : t = F.janet_wrap_number (Float.of_int x)
  let of_bool (b : bool) : t = F.janet_wrap_boolean (if b then 1 else 0)

  let of_string (s : string) : t =
    match F.janet_cstring s with
    | Some jstr -> F.janet_wrap_string jstr
    | None -> F.janet_wrap_nil ()
  ;;

  let of_keyword (s : string) : t = F.janet_wrap_keyword (F.janet_csymbol s)
  let of_symbol (s : string) : t = F.janet_wrap_symbol (F.janet_csymbol s)

  (** Return a human-readable description of a Janet value (equivalent to
    Janet's [(describe v)]). *)
  let to_string (v : t) : string = F.janet_description v

  (** Return the string form of a Janet value (equivalent to Janet's
    [(string v)]). For strings this gives the raw content; for other
    types it behaves like [to_string]. *)
  let to_string_value (v : t) : string = F.janet_to_string v

  (* -- Predicates / inspection -- *)

  type janet_type = T.janet_type

  let is_nil (v : t) : bool = not (phys_equal (F.janet_checktype v T.Nil) 0)
  let typeof (v : t) : T.janet_type = F.janet_type v
end

module Abstract = Janet_abstract.Make (Janet)
module Array_ = Janet_array.Make (Janet)
module Buffer_ = Janet_buffer.Make (Janet)
module Cfunction = Janet_cfunction.Make (Janet)
module Compile = Janet_compile.Make (Janet)
module Compile_result = Janet_compile_result.Make (Janet)
module Fiber = Janet_fiber.Make (Janet)
module Function = Janet_function.Make (Janet)
module Parser = Janet_parser.Make (Janet)
module Pointer = Janet_pointer.Make (Janet)
module Tuple_ = Janet_tuple.Make (Janet)
module Vm = Janet_vm.Make (Janet)
module Ev = Janet_ev.Make (Janet)
module Channel = Janet_channel.Make (Janet)
module Env = Env.Make (Janet)
module Marshal = Marshal.Make (Janet)
module Unwrapped = Unwrapped.Make (Janet)
module Cfun = Cfun.Make (Janet)

module Kv = struct
  include Janet_kv.Make (Janet)

  let sexp_of_t t =
    let k, v = to_pair t in
    List.sexp_of_t (fun x -> Unwrapped.of_janet x |> Unwrapped.sexp_of_t) [ k; v ]
  ;;
end

module Table = struct
  include Janet_table.Make (Janet)

  let sexp_of_t t = wrap t |> Unwrapped.of_janet |> Unwrapped.sexp_of_t
end

module Array = struct
  include Array_

  let sexp_of_t t = wrap t |> Unwrapped.of_janet |> Unwrapped.sexp_of_t
  let t_of_sexp s = Unwrapped.t_of_sexp s |> Unwrapped.to_janet |> unwrap
end

module Buffer = struct
  include Buffer_

  let sexp_of_t t = wrap t |> Unwrapped.of_janet |> Unwrapped.sexp_of_t
  let t_of_sexp s = Unwrapped.t_of_sexp s |> Unwrapped.to_janet |> unwrap
end

module Tuple = struct
  include Tuple_

  let sexp_of_t t = wrap t |> Unwrapped.of_janet |> Unwrapped.sexp_of_t
  let t_of_sexp s = Unwrapped.t_of_sexp s |> Unwrapped.to_janet |> unwrap
end

module Struct = struct
  include Janet_struct.Make (Janet)

  let sexp_of_t t = wrap t |> Unwrapped.of_janet |> Unwrapped.sexp_of_t
end

include Janet

let sexp_of_t t = Unwrapped.(sexp_of_t (of_janet t))
let t_of_sexp s = Unwrapped.(t_of_sexp s |> to_janet)

module F_top = Janet_c.C.Functions

(** Wrap an OCaml [int64] as a Janet s64 abstract value. *)
let wrap_s64 (x : int64) : t = F_top.janet_wrap_s64 x

(** Wrap an OCaml [uint64] as a Janet u64 abstract value. *)
let wrap_u64 (x : Unsigned.uint64) : t = F_top.janet_wrap_u64 x

(** Unwrap a Janet s64 value. Undefined behaviour if the value is not s64. *)
let unwrap_s64 (v : t) : int64 = F_top.janet_unwrap_s64 v

(** Unwrap a Janet u64 value. Undefined behaviour if the value is not u64. *)
let unwrap_u64 (v : t) : Unsigned.uint64 = F_top.janet_unwrap_u64 v

(** Pretty-print a Janet value into a string.
    [depth] controls the nesting depth for structures (default 4). *)
let pretty ?(depth = 4) (v : t) : string =
  let buf = Buffer.create 64 in
  let _ = F_top.janet_pretty buf depth 0 v in
  Buffer.to_string buf
;;

(* -- Generic operations  -- *)

let equal (a : t) (b : t) : bool = not (phys_equal (F_top.janet_equals a b) 0)
let length (v : t) : int = F_top.janet_length v |> Int32.to_int_exn
let get (v : t) (key : t) : t = F_top.janet_get v key
let put (v : t) ~(key : t) ~(value : t) : unit = F_top.janet_put v key value
let truthy (v : t) : bool = not (phys_equal (F_top.janet_truthy v) 0)
let gc_collect () : unit = F_top.janet_collect ()

(* -- Dynamic bindings  -- *)

let dyn (name : string) : t = F_top.janet_dyn name
let setdyn (name : string) (value : t) : unit = F_top.janet_setdyn name value

(* -- Signal checking -- *)

exception Janet_error = Janet_errors.Janet_error

let check_signal (signal : Fiber.signal) value : unit =
  match signal with
  | Fiber.Signal_ok -> ()
  | other ->
    raise
      (Janet_error
         (Printf.sprintf
            "Janet signal: %s (value: %s)"
            (Fiber.signal_to_string other)
            (Unwrapped.of_janet value |> Unwrapped.sexp_of_t |> Sexp.to_string_mach)))
;;

(* -- Eval -- *)

let dostring ?(source_path : string option) ~(env : Table.t) (str : string) =
  let out = create_ptr () in
  let raw = F_top.janet_dostring env str source_path (Some out) in
  let value = of_ptr out in
  let signal = if raw = 0 then Fiber.Signal_ok else Fiber.Signal_error in
  signal, value
;;

let dostring_exn ?(source_path : string option) ~env str =
  let signal, value =
    match source_path with
    | Some source_path -> dostring ~source_path ~env str
    | None -> dostring ~env str
  in
  check_signal signal value;
  value
;;

let dobytes ?(source_path : string option) ~(env : Table.t) (bytes : bytes) =
  let len = Bytes.length bytes in
  let c_arr = Ctypes.CArray.make Ctypes.uint8_t len in
  for i = 0 to len - 1 do
    Ctypes.CArray.set
      c_arr
      i
      (Unsigned.UInt8.of_int (Stdlib.Char.code (Bytes.get bytes i)))
  done;
  let out = create_ptr () in
  let raw =
    F_top.janet_dobytes
      env
      (Ctypes.CArray.start c_arr)
      (Int32.of_int_exn len)
      source_path
      (Some out)
  in
  let value = of_ptr out in
  let signal = if raw = 0 then Fiber.Signal_ok else Fiber.Signal_error in
  signal, value
;;

let dobytes_exn ?(source_path : string option) ~env bytes =
  let signal, value =
    match source_path with
    | Some source_path -> dobytes ~env bytes ~source_path
    | None -> dobytes ~env bytes
  in
  check_signal signal value;
  value
;;

(* -- Method calls -- *)

let mcall name (args : t list) =
  let argn = Int32.of_int_exn (List.length args) in
  let argv = Ctypes.CArray.of_list Janet_c.C.Types.janet args |> Ctypes.CArray.start in
  F_top.janet_mcall name argn argv
;;

let mcall_exn name (args : t list) =
  let value = mcall name args in
  let signal = F_top.janet_type value in
  ignore signal;
  value
;;

(* -- VM convenience -- *)

let with_janet_env (f : Env.t -> 'a) =
  Vm.with_vm (fun () ->
    let env = Env.core_env () in
    f env)
;;

(* Conversion *)

let to_keyword j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Keyword k -> Some k
       | _ -> None)
;;

let to_keyword_exn j = to_keyword j |> Option.value_exn ~message:"Not a keyword"

let to_number j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Number k -> Some k
       | _ -> None)
;;

let to_number_exn j = to_number j |> Option.value_exn ~message:"Not a number"

let to_function j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Function k -> Some k
       | _ -> None)
;;

let to_function_exn j = to_function j |> Option.value_exn ~message:"Not a function"

let to_bool j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Boolean b -> Some b
       | _ -> None)
;;

let to_bool_exn j = to_bool j |> Option.value_exn ~message:"Not a boolean"

let to_janet_string j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | String s -> Some s
       | _ -> None)
;;

let to_janet_string_exn j = to_janet_string j |> Option.value_exn ~message:"Not a string"

let to_symbol j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Symbol s -> Some s
       | _ -> None)
;;

let to_symbol_exn j = to_symbol j |> Option.value_exn ~message:"Not a symbol"

let to_array j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Array x -> Some x
       | _ -> None)
;;

let to_array_exn j = to_array j |> Option.value_exn ~message:"Not an array"

let to_tuple j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Tuple x -> Some x
       | _ -> None)
;;

let to_tuple_exn j = to_tuple j |> Option.value_exn ~message:"Not a tuple"

let to_table j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Table x -> Some x
       | _ -> None)
;;

let to_table_exn j = to_table j |> Option.value_exn ~message:"Not a table"

let to_struct j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Struct x -> Some x
       | _ -> None)
;;

let to_struct_exn j = to_struct j |> Option.value_exn ~message:"Not a struct"

let to_buffer j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Buffer x -> Some x
       | _ -> None)
;;

let to_buffer_exn j = to_buffer j |> Option.value_exn ~message:"Not a buffer"

let to_fiber j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Fiber x -> Some x
       | _ -> None)
;;

let to_fiber_exn j = to_fiber j |> Option.value_exn ~message:"Not a fiber"

let to_cfunction j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | CFunction x -> Some x
       | _ -> None)
;;

let to_cfunction_exn j = to_cfunction j |> Option.value_exn ~message:"Not a cfunction"

let to_int64 j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | Int x -> Some x
       | _ -> None)
;;

let to_int64_exn j = to_int64 j |> Option.value_exn ~message:"Not an int64"

let to_uint64 j =
  Unwrapped.of_janet j
  |> Unwrapped.(
       function
       | UInt x -> Some x
       | _ -> None)
;;

let to_uint64_exn j = to_uint64 j |> Option.value_exn ~message:"Not a uint64"
