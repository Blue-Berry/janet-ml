module Make (I : Janet_sig.S) = struct
  module F = Janet_c.C.Functions

  type t = Type.abstract

  let sexp_of_t _ = Core.Sexp.of_string "janet_abstract"
  let to_janet : t -> I.t = F.janet_wrap_abstract
end
