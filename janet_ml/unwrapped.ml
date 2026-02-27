open! Core
module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type janet = Janet.t

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
[@@deriving sexp_of]

type janet_type = T.janet_type

let check_type (janet : janet) = F.janet_type janet

let rec of_janet (janet : janet) =
  match F.janet_type janet with
  | T.Number -> Number (F.janet_unwrap_number janet)
  | T.Nil -> Nil
  | T.Boolean ->
    Boolean (if phys_equal (F.janet_unwrap_boolean janet) 0 then false else true)
  | T.Fiber -> Fiber (Janet_fiber.unwrap janet)
  | T.String -> String (F.janet_unwrap_string janet)
  | T.Symbol -> Symbol (F.janet_unwrap_symbol janet)
  | T.Keyword -> Keyword (F.janet_unwrap_keyword janet)
  | T.Array ->
    let arr = Ctypes.( !@ ) (F.janet_unwrap_array janet) in
    let count = Ctypes.getf arr T.Janet_Array.count |> Int32.to_int_exn in
    let data = Ctypes.getf arr T.Janet_Array.data in
    let arr =
      Ctypes.CArray.from_ptr data count
      |> Ctypes.CArray.to_list
      |> List.map ~f:(fun x -> of_janet x)
    in
    Array arr
  | T.Buffer ->
    let buf = Janet_buffer.unwrap janet in
    Buffer (Janet_buffer.contents buf)
  | T.Table -> Table (Janet_table.unwrap janet)
  | T.Struct -> Struct (Janet_struct.unwrap janet)
  | T.Function -> Function (F.janet_unwrap_function janet)
  | T.CFunction -> CFunction (F.janet_unwrap_cfunction janet)
  | T.Abstract -> Abstract (F.janet_unwrap_abstract janet)
  | T.Pointer -> Pointer (F.janet_unwrap_pointer janet)
  | T.Tuple ->
    let tup = Janet_tuple.unwrap janet in
    Tuple (Janet_tuple.to_list tup |> List.map ~f:of_janet)
;;
