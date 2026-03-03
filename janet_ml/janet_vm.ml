module F = Janet_c.C.Functions

module Make (_ : Janet_sig.S) = struct
  module T = Janet_c.C.Types

  type t = Type.vm

  let (local_vm : unit -> t) = F.janet_local_vm
  let (create : unit -> t) = F.janet_vm_alloc
  let (free : t -> unit) = F.janet_vm_free

  let save () : t =
    let vm = create () in
    F.janet_vm_save vm;
    vm
  ;;

  let load (vm : t) = F.janet_vm_load vm

  let init () =
    match F.janet_init () with
    | 0 -> ()
    | _ -> failwith "Failed to setup Janet Environment"
  ;;

  let deinit = F.janet_deinit

  let with_vm f =
    init ();
    Fun.protect ~finally:deinit f
  ;;
end

(* JANET_API void janet_interpreter_interrupt(JanetVM *vm); *)
(* JANET_API void janet_interpreter_interrupt_handled(JanetVM *vm); /* Call this after running interrupt handler */ *)
