module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type janet_table = [ `janet_table ] Ctypes.structure Ctypes_static.ptr
type janet = [ `janet ] Ctypes.structure Ctypes.ptr
type janet_fiber = [ `janet_fiber ] Ctypes.structure Ctypes_static.typ

type janet_type =
  | Number of float
  | Nil of unit
  | Boolean of bool
  | Fiber of janet_fiber
  | String of string
  | Symbol
  | Keyword
  | Array of janet_type array
  | Tuple of (janet_type * janet_type)
  | Table
  | Struct
  | Buffer
  | Function
  | CFunction
  | Abstract
  | Pointer

let janet_init () =
  match F.janet_init () with
  | 0 -> ()
  | _ -> failwith "Failed to setup Janet Enviroment"
;;

let janet_core_env (replacements : janet_table option) : janet_table =
  F.janet_core_env replacements
;;

let janet_dostring (env : janet_table) (str : string) (source_path : string option)
  : janet
  =
  let out = Ctypes.allocate_n T.janet ~count:1 in
  let _ = F.janet_dostring env str source_path (Some out) in
  out
;;

let janet_dobytes (env : janet_table) (bytes : bytes) (source_path : string option)
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
  out
;;

let janet_deinit = F.janet_deinit
