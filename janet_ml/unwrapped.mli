type t =
  | Number of float
  | Nil
  | Boolean of bool
  | Fiber of Janet_fiber.t
  | String of string
  | Symbol of string
  | Keyword of string
  | Array of t list
  | Tuple of t list
  | Table of Janet_table.t
  | Struct of Janet_struct.t
  | Buffer of bytes
  | Function of Janet_function.t
  | CFunction of Janet_cfunction.t
  | Abstract of Janet_abstract.t
  | Pointer of Janet_pointer.t

val sexp_of_t : t -> Sexplib0.Sexp.t
val of_janet : Janet.t -> t
