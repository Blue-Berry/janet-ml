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

type parse_error =
  { message : string
  ; line : int
  ; column : int
  }

let parse_all (s : string) : (Janet.t list, parse_error) result =
  let p = create () in
  let jv = to_janet p in
  F.janet_gcroot jv;
  consume_string p s;
  eof p;
  let rec collect acc =
    if has_more p
    then collect (produce p :: acc)
    else (
      match status p with
      | Root | Dead ->
        F.janet_gcunroot jv |> ignore;
        Ok (List.rev acc)
      | Error ->
        let msg = Option.value ~default:"unknown parse error" (error p) in
        let l = line p in
        let c = column p in
        F.janet_gcunroot jv |> ignore;
        Stdlib.Error { message = msg; line = l; column = c }
      | Pending ->
        F.janet_gcunroot jv |> ignore;
        Stdlib.Error
          { message = "unexpected end of input"; line = line p; column = column p })
  in
  collect []
;;

(* This is wrong *)
let compile_string ~(env : Janet_table.t) (s : string) ~(source : string)
  : (Janet_function.t list, Janet_compile.error) result
  =
  match parse_all s with
  | Stdlib.Error { message; line; column } ->
    Stdlib.Error { Janet_compile.message; line; column }
  | Ok forms ->
    let rec compile_forms acc = function
      | [] -> Ok (List.rev acc)
      | form :: rest ->
        (match Janet_compile.compile form env source with
         | Ok funcdef ->
           let fn = Janet_compile.to_function funcdef in
           compile_forms (fn :: acc) rest
         | Stdlib.Error e -> Stdlib.Error e)
    in
    compile_forms [] forms
;;
