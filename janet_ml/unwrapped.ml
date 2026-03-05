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
    | Array of Janet_array.t
    | Tuple of Janet_tuple.t
    | Table of Janet_table.t
    | Struct of Janet_struct.t
    | Buffer of bytes
    | Function of Janet_function.t
    | CFunction of Janet_cfunction.t
    | Int of int64
    | UInt of Unsigned.uint64
    | Abstract of Janet_abstract.t
    | Pointer of Janet_pointer.t

  let rec sexp_of_t : t -> Sexp.t = function
    | Number f -> Sexp.List [ Sexp.Atom "Number"; Float.sexp_of_t f ]
    | Nil -> Sexp.Atom "Nil"
    | Boolean b -> Sexp.List [ Sexp.Atom "Boolean"; Bool.sexp_of_t b ]
    | Fiber f -> Sexp.List [ Sexp.Atom "Fiber"; Janet_fiber.sexp_of_t f ]
    | String s -> Sexp.List [ Sexp.Atom "String"; String.sexp_of_t s ]
    | Symbol s -> Sexp.List [ Sexp.Atom "Symbol"; String.sexp_of_t s ]
    | Keyword s -> Sexp.List [ Sexp.Atom "Keyword"; String.sexp_of_t s ]
    | Array arr ->
      Sexp.List
        [ Sexp.Atom "Array"
        ; List.sexp_of_t sexp_of_t (Janet_array.to_list arr |> List.map ~f:of_janet)
        ]
    | Tuple tup ->
      Sexp.List
        [ Sexp.Atom "Tuple"
        ; List.sexp_of_t sexp_of_t (Janet_tuple.to_list tup |> List.map ~f:of_janet)
        ]
    | Table t ->
      let pairs =
        Janet_table.to_pairs t
        |> List.map ~f:(fun (k, v) ->
          Sexp.List [ of_janet k |> sexp_of_t; of_janet v |> sexp_of_t ])
      in
      Sexp.List [ Sexp.Atom "Table"; Sexp.List pairs ]
    | Struct s ->
      let pairs =
        Janet_struct.to_pairs s
        |> List.map ~f:(fun (k, v) ->
          Sexp.List [ of_janet k |> sexp_of_t; of_janet v |> sexp_of_t ])
      in
      Sexp.List [ Sexp.Atom "Struct"; Sexp.List pairs ]
    | Buffer b -> Sexp.List [ Sexp.Atom "Buffer"; Bytes.sexp_of_t b ]
    | Function f -> Sexp.List [ Sexp.Atom "Function"; Janet_function.sexp_of_t f ]
    | CFunction f -> Sexp.List [ Sexp.Atom "CFunction"; Janet_cfunction.sexp_of_t f ]
    | Int i -> Sexp.List [ Sexp.Atom "Int"; Int64.sexp_of_t i ]
    | UInt u ->
      Sexp.List [ Sexp.Atom "UInt"; Int64.sexp_of_t (Unsigned.UInt64.to_int64 u) ]
    | Abstract a -> Sexp.List [ Sexp.Atom "Abstract"; Janet_abstract.sexp_of_t a ]
    | Pointer p -> Sexp.List [ Sexp.Atom "Pointer"; Janet_pointer.sexp_of_t p ]

  and t_of_sexp (sexp : Sexp.t) : t =
    match sexp with
    | Atom "Nil" -> Nil
    | List [ Atom "Number"; n ] -> Number (Float.t_of_sexp n)
    | List [ Atom "Boolean"; b ] -> Boolean (Bool.t_of_sexp b)
    | List [ Atom "String"; s ] -> String (String.t_of_sexp s)
    | List [ Atom "Symbol"; s ] -> Symbol (String.t_of_sexp s)
    | List [ Atom "Keyword"; s ] -> Keyword (String.t_of_sexp s)
    | List [ Atom "Buffer"; b ] -> Buffer (Bytes.t_of_sexp b)
    | List [ Atom "Array"; List items ] ->
      Array (Janet_array.of_list (List.map ~f:(fun s -> t_of_sexp s |> to_janet) items))
    | List [ Atom "Tuple"; List items ] ->
      Tuple (Janet_tuple.of_list (List.map ~f:(fun s -> t_of_sexp s |> to_janet) items))
    | List [ Atom "Int"; i ] -> Int (Int64.t_of_sexp i)
    | List [ Atom "UInt"; u ] -> UInt (Unsigned.UInt64.of_int64 (Int64.t_of_sexp u))
    | _ -> Sexplib0.Sexp_conv_error.no_matching_variant_found "Unwrapped.t" sexp

  and to_janet (t : t) : janet =
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
    | Array arr -> Janet_array.wrap arr
    | Tuple tup -> Janet_tuple.wrap tup
    | Table tbl -> Janet_table.wrap tbl
    | Struct st -> Janet_struct.wrap st
    | Buffer b ->
      let buf = Janet_buffer.create (Bytes.length b) in
      Janet_buffer.push_bytes buf b;
      Janet_buffer.wrap buf
    | Function f -> F.janet_wrap_function f
    | CFunction f -> F.janet_wrap_cfunction f
    | Int i -> F.janet_wrap_s64 i
    | UInt u -> F.janet_wrap_u64 u
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
    | T.Array -> Array (Janet_array.unwrap janet)
    | T.Buffer ->
      let buf = Janet_buffer.unwrap janet in
      Buffer (Janet_buffer.contents buf)
    | T.Table -> Table (Janet_table.unwrap janet)
    | T.Struct -> Struct (Janet_struct.unwrap janet)
    | T.Function -> Function (F.janet_unwrap_function janet)
    | T.CFunction -> CFunction (F.janet_unwrap_cfunction janet)
    | T.Abstract ->
      (match F.janet_is_int janet with
       | T.Int_s64 -> Int (F.janet_unwrap_s64 janet)
       | T.Int_u64 -> UInt (F.janet_unwrap_u64 janet)
       | T.Int_none -> Abstract (F.janet_unwrap_abstract janet))
    | T.Pointer -> Pointer (F.janet_unwrap_pointer janet)
    | T.Tuple -> Tuple (Janet_tuple.unwrap janet)
  ;;

  type janet_type = T.janet_type

  let check_type (janet : janet) = F.janet_type janet
end
