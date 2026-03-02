module Make (_ : Janet_sig.S) = struct
  type t = Type.abstract

  let sexp_of_t _ = Core.Sexp.of_string "janet_abstract"
end
