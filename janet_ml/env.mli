type t = Janet_table.t

val core_env : replacements:Janet_table.t option -> t

module Binding : sig
  type env = t

  type t =
    | None
    | Def of Janet.t
    | Var of Janet.t
    | Macro of Janet.t
    | Dynamic_def of Janet.t
    | Dynamic_macro of Janet.t

  val lookup : env:env -> string -> t
  val to_janet : t -> Janet.t
end
