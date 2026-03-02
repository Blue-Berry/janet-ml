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
end

module Buffer = struct
  include Buffer_

  let sexp_of_t t = wrap t |> Unwrapped.of_janet |> Unwrapped.sexp_of_t
end

module Tuple = struct
  include Tuple_

  let sexp_of_t t = wrap t |> Unwrapped.of_janet |> Unwrapped.sexp_of_t
end

module Struct = struct
  include Janet_struct.Make (Janet)

  let sexp_of_t t = wrap t |> Unwrapped.of_janet |> Unwrapped.sexp_of_t
end

include Janet

let sexp_of_t t = Unwrapped.(sexp_of_t (of_janet t))
