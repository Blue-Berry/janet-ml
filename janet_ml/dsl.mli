module type S = sig
  val janet_lib : string

  type fun_t =
    { name : string
    ; f : Janet.t array -> Janet.t
    }

  val ext_funs : fun_t list
end

module Make : (S : S) -> sig
  exception Janet_error of string

  module Janet = Janet

  (** Run [f] with a fresh child environment whose parent is a persistent
      precomputed base environment (DSL + ext_funs loaded once). *)
  val with_env : (Janet.Env.t -> 'a) -> 'a
end
