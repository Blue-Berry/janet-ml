open! Core
module F = Janet_c.C.Functions
module T = Janet_c.C.Types
module Janet = Janet

let janet_init () =
  match F.janet_init () with
  | 0 -> ()
  | _ -> failwith "Failed to setup Janet Enviroment"
;;

let janet_deinit = F.janet_deinit

let janet_dostring ~(env : Janet.Table.t) (str : string) ~(source_path : string option)
  : Janet.t
  =
  let out = Janet.create_ptr () in
  let _ = F.janet_dostring env str source_path (Some out) in
  Janet.of_ptr out
;;

let janet_dobytes (env : Janet.Table.t) (bytes : bytes) (source_path : string option)
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
      (f : Janet.Function.t)
      (args : Janet.t list)
      ?(fiber : Janet.Fiber.t option = None)
      ()
  =
  let argn = Int32.of_int_exn (List.length args) in
  let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
  let out = Janet.create_ptr () in
  let fiber = Option.map ~f:(Ctypes.allocate (Ctypes.ptr T.Janet_Fiber.t)) fiber in
  let signal = F.janet_pcall f argn argv out fiber in
  signal, Janet.of_ptr out
;;

let call (f : Janet.Function.t) (args : Janet.t list) : Janet.t =
  let argn = Int32.of_int_exn (List.length args) in
  let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
  F.janet_call f argn argv
;;

let mcall name (args : Janet.t list) =
  let argn = Int32.of_int_exn (List.length args) in
  let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
  F.janet_mcall name argn argv
;;
