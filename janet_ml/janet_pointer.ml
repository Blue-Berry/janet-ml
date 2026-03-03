module Make (I : Janet_sig.S) = struct
  open! Core
  module F = Janet_c.C.Functions

  type t = Type.pointer

  let to_janet : t -> I.t = F.janet_wrap_pointer

  let sexp_of_t t =
    Sexp.List [ Sexp.Atom "Pointer"; to_janet t |> I.to_string |> Sexp.of_string ]
  ;;
end
