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
module Array = Janet_array.Make (Janet)
module Buffer = Janet_buffer.Make (Janet)
module Cfunction = Janet_cfunction.Make (Janet)
module Compile = Janet_compile.Make (Janet)
module Compile_result = Janet_compile_result.Make (Janet)
module Fiber = Janet_fiber.Make (Janet)
module Function = Janet_function.Make (Janet)
module Kv = Janet_kv.Make (Janet)
module Parser = Janet_parser.Make (Janet)
module Pointer = Janet_pointer.Make (Janet)
module Struct = Janet_struct.Make (Janet)
module Table = Janet_table.Make (Janet)
module Tuple = Janet_tuple.Make (Janet)
module Vm = Janet_vm.Make (Janet)
module Env = Env.Make (Janet)
module Marshal = Marshal.Make (Janet)
module Unwrapped = Unwrapped.Make (Janet)
include Janet

let sexp_of_t t = Unwrapped.(sexp_of_t (of_janet t))
