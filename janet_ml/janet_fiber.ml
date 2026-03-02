module Make (I : Janet_sig.S) = struct
  open! Core
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types
  module Janet_function = Janet_function.Make (I)
  module Janet_table = Janet_table.Make (I)

  type t = Type.fiber

  let stacktrace (fiber : t) (err : I.t) = F.janet_stacktrace fiber err

  type status =
    | Dead
    | Error
    | Debug
    | Pending
    | New
    | Alive
    | User of int
  [@@deriving sexp_of]

  type janet_signal = T.janet_signal

  let create (callee : Janet_function.t) ~capacity ~(argv : I.t list) : t =
    let argc = List.length argv in
    let c_arr = Ctypes.CArray.of_list T.janet argv in
    F.janet_fiber
      callee
      (Int32.of_int_exn capacity)
      (Int32.of_int_exn argc)
      (Ctypes.CArray.start c_arr)
  ;;

  let reset (fiber : t) (callee : Janet_function.t) ~(argv : I.t list) : t =
    let argc = List.length argv in
    let c_arr = Ctypes.CArray.of_list T.janet argv in
    F.janet_fiber_reset fiber callee (Int32.of_int_exn argc) (Ctypes.CArray.start c_arr)
  ;;

  let status (fiber : t) : status =
    match F.janet_fiber_status fiber with
    | T.Status_dead -> Dead
    | T.Status_error -> Error
    | T.Status_debug -> Debug
    | T.Status_pending -> Pending
    | T.Status_new -> New
    | T.Status_alive -> Alive
    | T.Status_user0 -> User 0
    | T.Status_user1 -> User 1
    | T.Status_user2 -> User 2
    | T.Status_user3 -> User 3
    | T.Status_user4 -> User 4
    | T.Status_user5 -> User 5
    | T.Status_user6 -> User 6
    | T.Status_user7 -> User 7
    | T.Status_user8 -> User 8
    | T.Status_user9 -> User 9
  ;;

  let sexp_of_t t = status t |> sexp_of_status
  let current () : t = F.janet_current_fiber ()
  let wrap (fiber : t) : I.t = F.janet_wrap_fiber fiber
  let unwrap (j : I.t) : t = F.janet_unwrap_fiber j

  let continue (fiber : t) (janet : I.t) : janet_signal * I.t =
    let out = I.create_ptr () in
    let signal = F.janet_continue fiber janet out in
    signal, I.of_ptr out
  ;;

  let continue_signal (fiber : t) (janet : I.t) (signal : janet_signal)
    : janet_signal * I.t
    =
    let out = I.create_ptr () in
    let signal = F.janet_continue_signal fiber janet out signal in
    signal, I.of_ptr out
  ;;

  (* JANET_API JanetSignal janet_step(JanetFiber *fiber, Janet in, Janet *out); *)
  let step (fiber : t) (janet : I.t) : janet_signal * I.t =
    let out = I.create_ptr () in
    let signal = F.janet_step fiber janet out in
    signal, I.of_ptr out
  ;;

  let set_env (t : t) (env : Janet_table.t) = Ctypes.(setf !@t T.Janet_Fiber.env env)

  (** Inject [msg] as an error into [fiber], transitioning it to the Error state.
      Equivalent to Janet's [(cancel fiber msg)]. The error propagates when the
      fiber is next resumed via [continue] or [step]. *)
  let cancel (fiber : t) (msg : I.t) : unit = F.janet_cancel fiber msg

  (** Returns true if the fiber can be resumed (status is New or Pending). *)
  let can_resume (fiber : t) : bool = F.janet_fiber_can_resume fiber <> 0
end
