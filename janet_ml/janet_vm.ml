module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type t = [ `janet_vm ] Ctypes.structure Ctypes_static.ptr

let (local_vm : unit -> t) = F.janet_local_vm
let (create : unit -> t) = F.janet_vm_alloc
let (free : t -> unit) = F.janet_vm_free

let save () : t =
  let vm = create () in
  F.janet_vm_save vm;
  vm
;;

let load (vm : t) = F.janet_vm_load vm

(* JANET_API void janet_interpreter_interrupt(JanetVM *vm); *)
(* JANET_API void janet_interpreter_interrupt_handled(JanetVM *vm); /* Call this after running interrupt handler */ *)
