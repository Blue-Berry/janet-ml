open! Core
module F = Janet_c.C.Functions
module T = Janet_c.C.Types
module Janet = Janet
module Janet_abstract = Janet_abstract
module Janet_buffer = Janet_buffer
module Janet_cfunction = Janet_cfunction
module Janet_compile = Janet_compile
module Janet_compile_result = Janet_compile_result
module Janet_fiber = Janet_fiber
module Janet_function = Janet_function
module Janet_parser = Janet_parser
module Janet_pointer = Janet_pointer
module Janet_struct = Janet_struct
module Janet_table = Janet_table
module Janet_tuple = Janet_tuple
module Janet_vm = Janet_vm
module Env = Env
module Marshal = Marshal
module Unwrapped = Unwrapped

let janet_init () =
  match F.janet_init () with
  | 0 -> ()
  | _ -> failwith "Failed to setup Janet Enviroment"
;;

let janet_deinit = F.janet_deinit

let janet_dostring ~(env : Janet_table.t) (str : string) ~(source_path : string option)
  : Janet.t
  =
  let out = Janet.create_ptr () in
  let _ = F.janet_dostring env str source_path (Some out) in
  Janet.of_ptr out
;;

let janet_dobytes (env : Janet_table.t) (bytes : bytes) (source_path : string option)
  : Janet.t
  =
  let len = Bytes.length bytes in
  let c_arr = Ctypes.CArray.make Ctypes.uint8_t len in
  for i = 0 to len - 1 do
    Ctypes.CArray.set
      c_arr
      i
      (Unsigned.UInt8.of_int (Stdlib.Char.code (Bytes.get bytes i)))
  done;
  let out = Janet.create_ptr () in
  let _ =
    F.janet_dobytes
      env
      (Ctypes.CArray.start c_arr)
      (Int32.of_int_exn len)
      source_path
      (Some out)
  in
  Janet.of_ptr out
;;

let pcall
      (f : Janet_function.t)
      (args : Janet.t list)
      ?(fiber : Janet_fiber.t option = None)
      ()
  =
  let argn = Int32.of_int_exn (List.length args) in
  let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
  let out = Janet.create_ptr () in
  let fiber = Option.map ~f:(Ctypes.allocate (Ctypes.ptr T.Janet_Fiber.t)) fiber in
  let signal = F.janet_pcall f argn argv out fiber in
  signal, Janet.of_ptr out
;;

let call (f : Janet_function.t) (args : Janet.t list) : Janet.t =
  let argn = Int32.of_int_exn (List.length args) in
  let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
  F.janet_call f argn argv
;;

let mcall name (args : Janet.t list) =
  let argn = Int32.of_int_exn (List.length args) in
  let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
  F.janet_mcall name argn argv
;;
