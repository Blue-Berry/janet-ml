module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type t = [ `janet_compile_result ] Ctypes.structure

type status =
  | Ok
  | Error

let funcdef (res : t) : [ `janet_funcdef ] Ctypes.structure Ctypes_static.ptr =
  Ctypes.getf res T.Janet_Compile_Result.funcdef
;;

let error (res : t) : Unsigned.UInt8.t Ctypes_static.ptr =
  Ctypes.getf res T.Janet_Compile_Result.error
;;

let error_string (res : t) : string =
  let err_ptr = Ctypes.getf res T.Janet_Compile_Result.error in
  let head_offset = Ctypes.sizeof T.Janet_String.head in
  let p = Ctypes.from_voidp Ctypes.char (Ctypes.to_voidp err_ptr) in
  let head =
    Ctypes.from_voidp T.Janet_String.head (Ctypes.to_voidp Ctypes.(p +@ -head_offset))
  in
  let len = Ctypes.getf Ctypes.(!@head) T.Janet_String.length |> Int32.to_int in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    let c = Ctypes.(!@(err_ptr +@ i)) |> Unsigned.UInt8.to_int |> Char.chr in
    Buffer.add_char buf c
  done;
  Buffer.contents buf
;;

let macrofiber (res : t) : Janet_fiber.t =
  Ctypes.getf res T.Janet_Compile_Result.macrofiber
;;

let error_mapping_line (res : t) : int =
  let mapping = Ctypes.getf res T.Janet_Compile_Result.error_mapping in
  Ctypes.getf mapping T.janet_source_mapping_line |> Int32.to_int
;;

let error_mapping_column (res : t) : int =
  let mapping = Ctypes.getf res T.Janet_Compile_Result.error_mapping in
  Ctypes.getf mapping T.janet_source_mapping_column |> Int32.to_int
;;

let status (res : t) : status =
  match Ctypes.getf res T.Janet_Compile_Result.status with
  | T.Compile_ok -> Ok
  | T.Compile_error -> Error
;;
