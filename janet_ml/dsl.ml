module Janet = Janet

module type S = sig
  val janet_lib : string

  type fun_t =
    { name : string
    ; f : Janet.t array -> Janet.t
    }

  val ext_funs : fun_t list
end

module Make (S : S) = struct
  exception Janet_error = Janet_errors.Janet_error

  module Janet = Janet

  let replacements =
    Janet.with_janet_env (fun env ->
      let _ = Janet.dostring_exn ~env S.janet_lib ~source_path:None in
      List.iter
        (fun f -> Janet.Cfun.register ~env S.(f.name) S.(f.f) |> ignore)
        S.ext_funs;
      Janet.Env.core_env ~replacements:env ())
  ;;

  let with_env (f : Janet.Env.t -> 'a) =
    Janet.Vm.with_vm (fun () ->
      let env = Janet.Env.core_env ~replacements () in
      f env)
  ;;
end
