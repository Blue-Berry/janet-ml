module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type t = [ `janet_parser ] Ctypes.structure Ctypes_static.ptr

type status =
  | Root
  | Error
  | Pending
  | Dead

let status_of_c = function
  | T.Parse_root -> Root
  | T.Parse_error -> Error
  | T.Parse_pending -> Pending
  | T.Parse_dead -> Dead
;;

let create () : t =
  let at = F.janet_parser_type in
  let sz = Unsigned.Size_t.of_int (Ctypes.sizeof T.Janet_Parser.t) in
  let vp = F.janet_abstract at sz in
  let p = Ctypes.from_voidp T.Janet_Parser.t vp in
  F.janet_parser_init p;
  p
;;

let to_janet (p : t) : Janet.t = F.janet_wrap_abstract (Ctypes.to_voidp p)

let consume (p : t) (c : char) : unit =
  F.janet_parser_consume p (Unsigned.UInt8.of_int (Char.code c))
;;

let consume_string (p : t) (s : string) : unit = String.iter (fun c -> consume p c) s
let eof (p : t) : unit = F.janet_parser_eof p
let has_more (p : t) : bool = F.janet_parser_has_more p <> 0
let produce (p : t) : Janet.t = F.janet_parser_produce p
let status (p : t) : status = status_of_c (F.janet_parser_status p)
let error (p : t) : string option = F.janet_parser_error p
let flush (p : t) : unit = F.janet_parser_flush p

let line (p : t) : int =
  Ctypes.getf Ctypes.(!@p) T.Janet_Parser.line |> Unsigned.Size_t.to_int
;;

let column (p : t) : int =
  Ctypes.getf Ctypes.(!@p) T.Janet_Parser.column |> Unsigned.Size_t.to_int
;;
