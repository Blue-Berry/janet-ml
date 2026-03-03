module type S = sig
  val lib : string
end

(* module Make (S : S) = struct *)
(*   let replacements = *)
(*     with_janet_env (fun env -> *)
(*       let _ = Janet_ml.dostring_exn ~env S.lib ~source_path:None in *)
(*       Janet.Env.core_env ~replacements:(Some env)) *)
(*   ;; *)
(*   let with_env (f : Janet.Env.t -> 'a) = *)
(*     with_janet (fun () -> *)
(*       let env = Janet.Env.core_env ~replacements in *)
(*       f env) *)
(*   ;; *)
(* end *)
