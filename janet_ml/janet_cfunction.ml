type t =
  (int32 -> [ `janet ] Ctypes.structure Ctypes_static.ptr -> [ `janet ] Ctypes.structure)
    Ctypes_static.static_funptr

let sexp_of_t _ = Core.Sexp.of_string "janet_cfunction"
