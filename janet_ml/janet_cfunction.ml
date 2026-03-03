open! Core

module Make (I : Janet_sig.S) = struct
  module F = Janet_c.C.Functions

  type t = Type.cfunction

  let to_janet : t -> I.t = F.janet_wrap_cfunction
  let sexp_of_t t = Sexp.Atom (to_janet t |> I.to_string)
end
