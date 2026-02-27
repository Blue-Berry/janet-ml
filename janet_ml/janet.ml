open! Core
module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type t = [ `janet ] Ctypes.structure
type ptr = t Ctypes.ptr

let of_ptr : ptr -> t = Ctypes.( !@ )
let to_ptr : t -> ptr = Ctypes.addr
let create () : t = F.janet_wrap_nil ()
let create_ptr () : ptr = Ctypes.addr (create ())
