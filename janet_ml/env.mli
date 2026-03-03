module Make (I : Janet_sig.S) : sig
  type t = Type.table

  val core_env : replacements:t option -> t

  (** Bind [name] to [value] as an immutable def in [env]. *)
  val def : t -> ?doc:string -> string -> I.t -> unit

  (** Bind [name] to [value] as a mutable var in [env]. *)
  val var : t -> ?doc:string -> string -> I.t -> unit

  module Binding : sig
    type env = t

    type t =
      | None
      | Def of I.t
      | Var of I.t
      | Macro of I.t
      | Dynamic_def of I.t
      | Dynamic_macro of I.t

    val lookup : env:env -> string -> t
    val to_janet : t -> I.t
  end
end
