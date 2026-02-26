type t = unit Ctypes_static.ptr

let sexp_of_t _ = Core.Sexp.of_string "janet_pointer"
