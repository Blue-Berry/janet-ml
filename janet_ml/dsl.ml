module Janet = Janet

module type S = sig
  val janet_lib : string

  type fun_t =
    { name : string
    ; f : Janet.t array -> Janet.t
    }

  val ext_funs : fun_t list
end

let vm_started = ref false

let ensure_vm_started () =
  if not !vm_started
  then (
    Janet.Vm.init ();
    vm_started := true;
    at_exit Janet.Vm.deinit)
;;

module Make (S : S) = struct
  exception Janet_error = Janet_errors.Janet_error

  module Janet = Janet

  type persistent_state =
    { base_env : Janet.Env.t
    ; handles : Janet.Cfun.handle list
    }

  let state : persistent_state Lazy.t =
    lazy
      (ensure_vm_started ();
       let base_env = Janet.Env.core_env () in
       let rooted_base_env = Janet.Table.wrap base_env in
       Janet.gc_root rooted_base_env;
       let _ = Janet.dostring_exn ~env:base_env S.janet_lib ~source_path:None in
       let handles =
         List.map
           (fun (fn : S.fun_t) -> Janet.Cfun.register ~env:base_env fn.name fn.f)
           S.ext_funs
       in
       at_exit (fun () ->
         List.iter Janet.Cfun.free handles;
         Janet.gc_unroot rooted_base_env);
       { base_env; handles })
  ;;

  let with_env (f : Janet.Env.t -> 'a) =
    let st = Lazy.force state in
    let env =
      Janet.dostring_exn ~env:st.base_env "(make-env root-env)" ~source_path:None
      |> Janet.Table.unwrap
    in
    Janet.with_root (Janet.Table.wrap env) ~f:(fun _ -> f env)
  ;;
end
