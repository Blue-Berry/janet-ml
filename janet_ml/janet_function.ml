module Make (I : Janet_sig.S) = struct
  open! Core
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types

  type t = Type.function_t

  let sexp_of_t _ = Sexp.of_string "janet_function"

  let pcall (f : t) (args : I.t list) ?(fiber : Type.fiber option = None) () =
    let argn = Int32.of_int_exn (List.length args) in
    let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
    let out = I.create_ptr () in
    let fiber = Option.map ~f:(Ctypes.allocate (Ctypes.ptr T.Janet_Fiber.t)) fiber in
    let signal = F.janet_pcall f argn argv out fiber in
    signal, I.of_ptr out
  ;;

  let call (f : t) (args : I.t list) : I.t =
    let argn = Int32.of_int_exn (List.length args) in
    let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
    F.janet_call f argn argv
  ;;
end
