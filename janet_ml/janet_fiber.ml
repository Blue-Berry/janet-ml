module F = Janet_c.C.Functions
module T = Janet_c.C.Types
open Janet_types

type t = [ `janet_fiber ] Ctypes.structure Ctypes_static.ptr

type status =
  | Dead
  | Error
  | Debug
  | Pending
  | New
  | Alive
  | User of int

let create (callee : janet_function) ~capacity ~(argv : janet list) : t =
  let argc = List.length argv in
  let c_arr = Ctypes.CArray.of_list T.janet argv in
  F.janet_fiber
    callee
    (Int32.of_int capacity)
    (Int32.of_int argc)
    (Ctypes.CArray.start c_arr)
;;

let reset (fiber : t) (callee : janet_function) ~(argv : janet list) : t =
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
let wrap (fiber : t) : janet = F.janet_wrap_fiber fiber
let unwrap (j : janet) : t = F.janet_unwrap_fiber j
