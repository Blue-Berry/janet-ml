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

  (* -- Extraction (option-returning) -- *)

  let to_float (v : t) : float option =
    match F.janet_type v with
    | T.Number -> Some (F.janet_unwrap_number v)
    | _ -> None
  ;;

  let to_int (v : t) : int option =
    match F.janet_type v with
    | T.Number -> Some (Float.to_int (F.janet_unwrap_number v))
    | _ -> None
  ;;

  let to_bool (v : t) : bool option =
    match F.janet_type v with
    | T.Boolean -> Some (not (phys_equal (F.janet_unwrap_boolean v) 0))
    | _ -> None
  ;;

  let to_string_opt (v : t) : string option =
    match F.janet_type v with
    | T.String -> Some (F.janet_unwrap_string v)
    | _ -> None
  ;;

  let to_keyword (v : t) : string option =
    match F.janet_type v with
    | T.Keyword -> Some (F.janet_unwrap_keyword v)
    | _ -> None
  ;;

  let to_symbol (v : t) : string option =
    match F.janet_type v with
    | T.Symbol -> Some (F.janet_unwrap_symbol v)
    | _ -> None
  ;;

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
module Env = Env.Make (Janet)
module Marshal = Marshal.Make (Janet)
module Unwrapped = Unwrapped.Make (Janet)

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

(** Return a human-readable description of a Janet value (equivalent to
    Janet's [(describe v)]). *)
let to_string (v : t) : string = F_top.janet_description v

(** Return the string form of a Janet value (equivalent to Janet's
    [(string v)]). For strings this gives the raw content; for other
    types it behaves like [to_string]. *)
let to_string_value (v : t) : string = F_top.janet_to_string v

(** Pretty-print a Janet value into a string.
    [depth] controls the nesting depth for structures (default 4). *)
let pretty ?(depth = 4) (v : t) : string =
  let buf = Buffer_.create 64 in
  let _ = F_top.janet_pretty buf depth 0 v in
  Buffer_.to_string buf
;;

(* -- Generic operations (Phase 1b) -- *)

let equal (a : t) (b : t) : bool = not (phys_equal (F_top.janet_equals a b) 0)
let length (v : t) : int = F_top.janet_length v |> Int32.to_int_exn
let get (v : t) (key : t) : t = F_top.janet_get v key
let put (v : t) ~(key : t) ~(value : t) : unit = F_top.janet_put v key value
let truthy (v : t) : bool = not (phys_equal (F_top.janet_truthy v) 0)
let gc_collect () : unit = F_top.janet_collect ()

(* -- Dynamic bindings (Phase 1d) -- *)

let dyn (name : string) : t = F_top.janet_dyn name
let setdyn (name : string) (value : t) : unit = F_top.janet_setdyn name value
