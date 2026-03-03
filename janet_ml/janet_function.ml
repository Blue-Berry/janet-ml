module Make (I : Janet_sig.S) = struct
  open! Core
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types

  type t = Type.function_t

  let (to_janet : t -> I.t) = F.janet_wrap_function

  let sexp_of_t t =
    Sexp.List [ Sexp.Atom "Function"; to_janet t |> I.to_string |> Sexp.of_string ]
  ;;

  let pcall (f : t) (args : I.t list) ?(fiber : Type.fiber option = None) () =
    let argn = Int32.of_int_exn (List.length args) in
    let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
    let out = I.create_ptr () in
    let fiber = Option.map ~f:(Ctypes.allocate (Ctypes.ptr T.Janet_Fiber.t)) fiber in
    let signal = F.janet_pcall f argn argv out fiber in
    signal, I.of_ptr out
  ;;

  (** Safe call: uses [pcall] internally and raises [Janet_errors.Janet_error]
      on any non-ok signal. Prefer this over [call] for production code. *)
  let call_exn (f : t) (args : I.t list) : I.t =
    let signal, value = pcall f args () in
    (match signal with
     | T.Signal_ok -> ()
     | T.Signal_error ->
       raise (Janet_errors.Janet_error "Janet function call raised an error")
     | T.Signal_debug ->
       raise (Janet_errors.Janet_error "Janet function call raised debug signal")
     | T.Signal_yield ->
       raise (Janet_errors.Janet_error "Janet function call yielded unexpectedly"));
    value
  ;;

  (** Direct call via [janet_call]. Note: a Janet panic in the called function
      will terminate the process. Prefer [call_exn] or [pcall] for safety. *)
  let call (f : t) (args : I.t list) : I.t =
    let argn = Int32.of_int_exn (List.length args) in
    let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
    F.janet_call f argn argv
  ;;
end
