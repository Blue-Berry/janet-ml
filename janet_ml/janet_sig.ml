module type S = sig
  type t = Type.janet
  type ptr = t Ctypes.ptr

  val of_ptr : ptr -> t
  val create_ptr : unit -> ptr
  val create : unit -> t
  val to_string : t -> string
  val to_string_value : t -> string
end
