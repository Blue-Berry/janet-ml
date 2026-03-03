module Make (I : Janet_sig.S) = struct
  open! Core
  module T = Janet_c.C.Types
  module F = Janet_c.C.Functions

  type t = Type.kv

  let key (kv : t) : I.t = Ctypes.getf Ctypes.(!@kv) T.janet_kv_key
  let value (kv : t) : I.t = Ctypes.getf Ctypes.(!@kv) T.janet_kv_value
  let is_null (kv : t) : bool = Ctypes.is_null kv
  let to_pair (kv : t) : I.t * I.t = key kv, value kv

  let sexp_of_t t =
    sexp_of_pair
      (fun x -> I.to_string x |> sexp_of_string)
      (fun x -> I.to_string x |> sexp_of_string)
      (to_pair t)
  ;;
end
