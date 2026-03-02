module Make (_ : Janet_sig.S) = struct
  type t = Type.cfunction

  let sexp_of_t _ = Core.Sexp.of_string "janet_cfunction"
end
