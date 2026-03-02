module Make (I : Janet_sig.S) = struct
  open! Core
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types
  module Janet_abstract = Janet_abstract.Make (I)
  module Janet_array = Janet_array.Make (I)
  module Janet_buffer = Janet_buffer.Make (I)
  module Janet_cfunction = Janet_cfunction.Make (I)
  module Janet_fiber = Janet_fiber.Make (I)
  module Janet_function = Janet_function.Make (I)
  module Janet_pointer = Janet_pointer.Make (I)
  module Janet_struct = Janet_struct.Make (I)
  module Janet_table = Janet_table.Make (I)
  module Janet_tuple = Janet_tuple.Make (I)

  type janet = I.t

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

  let rec t_of_sexp (sexp : Sexp.t) : t =
    match sexp with
    | Atom "Nil" -> Nil
    | List [ Atom "Number"; n ] -> Number (Float.t_of_sexp n)
    | List [ Atom "Boolean"; b ] -> Boolean (Bool.t_of_sexp b)
    | List [ Atom "String"; s ] -> String (String.t_of_sexp s)
    | List [ Atom "Symbol"; s ] -> Symbol (String.t_of_sexp s)
    | List [ Atom "Keyword"; s ] -> Keyword (String.t_of_sexp s)
    | List [ Atom "Buffer"; b ] -> Buffer (Bytes.t_of_sexp b)
    | List [ Atom "Array"; List items ] -> Array (List.map ~f:t_of_sexp items)
    | List [ Atom "Tuple"; List items ] -> Tuple (List.map ~f:t_of_sexp items)
    | _ -> Sexplib0.Sexp_conv_error.no_matching_variant_found "Unwrapped.t" sexp
  ;;

  type janet_type = T.janet_type

  let check_type (janet : janet) = F.janet_type janet

  let rec to_janet (t : t) : janet =
    match t with
    | Number f -> F.janet_wrap_number f
    | Nil -> F.janet_wrap_nil ()
    | Boolean b -> F.janet_wrap_boolean (if b then 1 else 0)
    | Fiber f -> Janet_fiber.wrap f
    | String s ->
      (match F.janet_cstring s with
       | Some jstr -> F.janet_wrap_string jstr
       | None -> F.janet_wrap_nil ())
    | Symbol s -> F.janet_wrap_symbol (F.janet_csymbol s)
    | Keyword s -> F.janet_wrap_keyword (F.janet_csymbol s)
    | Array items ->
      let arr = Janet_array.create (List.length items) in
      List.iter ~f:(fun x -> Janet_array.push arr (to_janet x)) items;
      Janet_array.wrap arr
    | Tuple items ->
      let tup = Janet_tuple.of_list (List.map ~f:to_janet items) in
      Janet_tuple.wrap tup
    | Table tbl -> Janet_table.wrap tbl
    | Struct st -> Janet_struct.wrap st
    | Buffer b ->
      let buf = Janet_buffer.create (Bytes.length b) in
      Janet_buffer.push_bytes buf b;
      Janet_buffer.wrap buf
    | Function f -> F.janet_wrap_function f
    | CFunction f -> F.janet_wrap_cfunction f
    | Abstract a -> F.janet_wrap_abstract a
    | Pointer p -> F.janet_wrap_pointer p

  and of_janet (janet : janet) =
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
      let arr = Janet_array.unwrap janet in
      Array (Janet_array.to_list arr |> List.map ~f:of_janet)
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
end
