module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type t = [ `janet ] Ctypes.structure
type ptr = t Ctypes.ptr

let of_ptr : ptr -> t = Ctypes.( !@ )
let to_ptr : t -> ptr = Ctypes.addr
let create_ptr () : ptr = Ctypes.allocate_n T.janet ~count:1
let create () : t = Ctypes.allocate_n T.janet ~count:1 |> of_ptr
