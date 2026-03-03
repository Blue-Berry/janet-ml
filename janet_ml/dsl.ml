module type S = sig
  val lib : string
end

module Make (S : S) = struct
  exception Janet_error = Janet_errors.Janet_error

  module Janet = Janet

  let replacements =
    Janet.with_janet_env (fun env ->
      let _ = Janet.dostring_exn ~env S.lib ~source_path:None in
      Janet.Env.core_env ~replacements:env ())
  ;;

  let with_env (f : Janet.Env.t -> 'a) =
    Janet.Vm.with_vm (fun () ->
      let env = Janet.Env.core_env ~replacements () in
      f env)
  ;;
end
