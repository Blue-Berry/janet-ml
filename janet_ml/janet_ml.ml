module F = Janet_c.C.Functions
module T = Janet_c.C.Types
open Janet_types

let janet_init () =
  match F.janet_init () with
  | 0 -> ()
  | _ -> failwith "Failed to setup Janet Enviroment"
;;

let janet_core_env (replacements : Janet_table.t option) : Janet_table.t =
  F.janet_core_env replacements
;;

let janet_dostring (env : Janet_table.t) (str : string) (source_path : string option)
  : janet
  =
  let out = Ctypes.allocate_n T.janet ~count:1 in
  let _ = F.janet_dostring env str source_path (Some out) in
  Ctypes.( !@ ) out
;;

let janet_dobytes (env : Janet_table.t) (bytes : bytes) (source_path : string option)
  : janet
  =
  let len = Bytes.length bytes in
  let c_arr = Ctypes.CArray.make Ctypes.uint8_t len in
  for i = 0 to len - 1 do
    Ctypes.CArray.set c_arr i (Unsigned.UInt8.of_int (Char.code (Bytes.get bytes i)))
  done;
  let out = Ctypes.allocate_n T.janet ~count:1 in
  let _ =
    F.janet_dobytes
      env
      (Ctypes.CArray.start c_arr)
      (Int32.of_int len)
      source_path
      (Some out)
  in
  Ctypes.( !@ ) out
;;

let janet_deinit = F.janet_deinit

module Janet_type = struct
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
    | Struct of janet_struct
    | Buffer of bytes
    | Function of janet_function
    | CFunction of janet_cfunction
    | Abstract of janet_abstract
    | Pointer of janet_pointer

  let check_type (janet : janet) = F.janet_type janet

  let rec of_janet (janet : janet) =
    match F.janet_type janet with
    | T.Number -> Number (F.janet_unwrap_number janet)
    | T.Nil -> Nil
    | T.Boolean -> Boolean (if F.janet_unwrap_boolean janet == 0 then false else true)
    | T.Fiber -> Fiber (Janet_fiber.unwrap janet)
    | T.String -> String (F.janet_unwrap_string janet)
    | T.Symbol -> Symbol (F.janet_unwrap_symbol janet)
    | T.Keyword -> Keyword (F.janet_unwrap_keyword janet)
    | T.Array ->
      let arr = Ctypes.( !@ ) (F.janet_unwrap_array janet) in
      let count = Ctypes.getf arr T.Janet_Array.count |> Int32.to_int in
      let data = Ctypes.getf arr T.Janet_Array.data in
      let arr =
        Ctypes.CArray.from_ptr data count
        |> Ctypes.CArray.to_list
        |> List.map (fun x -> of_janet x)
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
      Tuple (Janet_tuple.to_list tup |> List.map of_janet)
  ;;
end
