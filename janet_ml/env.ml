open! Core
module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type t = Janet_table.t

module Binding = struct
  type env = t

  type t =
    | None
    | Def of Janet.t
    | Var of Janet.t
    | Macro of Janet.t
    | Dynamic_def of Janet.t
    | Dynamic_macro of Janet.t

  let lookup ~(env : env) symbol =
    let out = Janet.create_ptr () in
    match F.janet_resolve env (F.janet_csymbol symbol) out with
    | T.Binding_none -> None
    | T.Binding_def -> Def (Janet.of_ptr out)
    | T.Binding_var -> Var (Janet.of_ptr out)
    | T.Binding_macro -> Macro (Janet.of_ptr out)
    | T.Binding_dynamic_def -> Dynamic_def (Janet.of_ptr out)
    | T.Binding_dynamic_macro -> Dynamic_macro (Janet.of_ptr out)
  ;;

  let to_janet (t : t) =
    match t with
    | None -> Janet.create ()
    | Def j -> j
    | Var j -> j
    | Macro j -> j
    | Dynamic_def j -> j
    | Dynamic_macro j -> j
  ;;
end
