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

  val with_env : (Janet.Env.t -> 'a) -> 'a
end
