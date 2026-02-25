module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type t = [ `janet_fiber ] Ctypes.structure Ctypes_static.ptr

type status =
  | Dead
  | Error
  | Debug
  | Pending
  | New
  | Alive
  | User of int

type janet_signal = T.janet_signal

let create (callee : Janet_function.t) ~capacity ~(argv : Janet.t list) : t =
  let argc = List.length argv in
  let c_arr = Ctypes.CArray.of_list T.janet argv in
  F.janet_fiber
    callee
    (Int32.of_int capacity)
    (Int32.of_int argc)
    (Ctypes.CArray.start c_arr)
;;

let reset (fiber : t) (callee : Janet_function.t) ~(argv : Janet.t list) : t =
  let argc = List.length argv in
  let c_arr = Ctypes.CArray.of_list T.janet argv in
  F.janet_fiber_reset fiber callee (Int32.of_int argc) (Ctypes.CArray.start c_arr)
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

let current () : t = F.janet_current_fiber ()
let wrap (fiber : t) : Janet.t = F.janet_wrap_fiber fiber
let unwrap (j : Janet.t) : t = F.janet_unwrap_fiber j

let continue (fiber : t) (janet : Janet.t) : janet_signal * Janet.t =
  let out = Janet.create_ptr () in
  let signal = F.janet_continue fiber janet out in
  signal, Janet.of_ptr out
;;

let continue_signal (fiber : t) (janet : Janet.t) (signal : janet_signal)
  : janet_signal * Janet.t
  =
  let out = Janet.create_ptr () in
  let signal = F.janet_continue_signal fiber janet out signal in
  signal, Janet.of_ptr out
;;

(* JANET_API JanetSignal janet_step(JanetFiber *fiber, Janet in, Janet *out); *)
let step (fiber : t) (janet : Janet.t) : janet_signal * Janet.t =
  let out = Janet.create_ptr () in
  let signal = F.janet_step fiber janet out in
  signal, Janet.of_ptr out
;;
