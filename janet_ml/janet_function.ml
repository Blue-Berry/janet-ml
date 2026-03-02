open! Core

type t = Type.function_t

let sexp_of_t _ = Sexp.of_string "janet_function"
