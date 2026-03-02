module Make (I : Janet_sig.S) = struct
  open! Core
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types
  module Janet_table = Janet_table.Make (I)

  type t = Janet_table.t

  let core_env ~(replacements : t option) : t = F.janet_core_env replacements

  module Binding = struct
    type env = t

    type t =
      | None
      | Def of I.t
      | Var of I.t
      | Macro of I.t
      | Dynamic_def of I.t
      | Dynamic_macro of I.t

    let lookup ~(env : env) symbol =
      let out = I.create_ptr () in
      match F.janet_resolve env (F.janet_csymbol symbol) out with
      | T.Binding_none -> None
      | T.Binding_def -> Def (I.of_ptr out)
      | T.Binding_var -> Var (I.of_ptr out)
      | T.Binding_macro -> Macro (I.of_ptr out)
      | T.Binding_dynamic_def -> Dynamic_def (I.of_ptr out)
      | T.Binding_dynamic_macro -> Dynamic_macro (I.of_ptr out)
    ;;

    let to_janet (t : t) =
      match t with
      | None -> I.create ()
      | Def j -> j
      | Var j -> j
      | Macro j -> j
      | Dynamic_def j -> j
      | Dynamic_macro j -> j
    ;;
  end
end
