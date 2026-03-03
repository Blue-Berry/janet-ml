open! Core
module F = Janet_c.C.Functions
module T = Janet_c.C.Types

(** The C type of a Janet C-function: [Janet f(int32_t argc, Janet *argv)] *)
let cfun_fn_type = Ctypes.(int32_t @-> ptr T.janet @-> returning T.janet)

(** Module providing explicit-lifetime function pointers for OCaml->Janet callbacks. *)
module JanetCfun = (val Foreign.dynamic_funptr cfun_fn_type)

module Make (I : Janet_sig.S) = struct
  module Env = Env.Make (I)
  module JanetCfun = JanetCfun

  type handle = JanetCfun.t

  let free = JanetCfun.free

  let register ~(env : Type.table) (name : string) (f : I.t array -> I.t) : handle =
    let raw_f argc argv =
      let n = Int32.to_int_exn argc in
      let args =
        Ctypes.CArray.from_ptr argv n |> Ctypes.CArray.to_list |> Array.of_list
      in
      f args
    in
    let dyn = JanetCfun.of_fun raw_f in
    let sfptr = Ctypes.coerce JanetCfun.t (Ctypes.static_funptr cfun_fn_type) dyn in
    let jval = F.janet_wrap_cfunction sfptr in
    Env.def env name jval;
    dyn
  ;;

  let register_raw
        ~(env : Type.table)
        (name : string)
        (f : int32 -> I.t Ctypes.ptr -> I.t)
    : handle
    =
    let dyn = JanetCfun.of_fun f in
    let sfptr = Ctypes.coerce JanetCfun.t (Ctypes.static_funptr cfun_fn_type) dyn in
    let jval = F.janet_wrap_cfunction sfptr in
    Env.def env name jval;
    dyn
  ;;
end
