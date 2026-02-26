open! Core

type t = [ `janet_function ] Ctypes.structure Ctypes_static.ptr

let sexp_of_t _ = Sexp.of_string "janet_function"
