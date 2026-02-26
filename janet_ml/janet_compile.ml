module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type funcdef = [ `janet_funcdef ] Ctypes.structure Ctypes_static.ptr

type error =
  { message : string
  ; line : int
  ; column : int
  }

type result = (funcdef, error) Stdlib.result

let extract_result res =
  match Janet_compile_result.status res with
  | Janet_compile_result.Ok -> Ok (Janet_compile_result.funcdef res)
  | Janet_compile_result.Error ->
    let line = Janet_compile_result.error_mapping_line res in
    let column = Janet_compile_result.error_mapping_column res in
    let message = Janet_compile_result.error_string res in
    Error { message; line; column }
;;

let compile (source : Janet.t) (env : Janet_table.t) (where : string) : result =
  match F.janet_cstring where with
  | Some jstr ->
    let res = F.janet_compile source env jstr in
    extract_result res
  | None -> Error { message = "failed to create janet string"; line = 0; column = 0 }
;;

let compile_lint
      (source : Janet.t)
      (env : Janet_table.t)
      (where : string)
      (lints : [ `janet_array ] Ctypes.structure Ctypes_static.ptr)
  : result
  =
  match F.janet_cstring where with
  | Some jstr ->
    let res = F.janet_compile_lint source env jstr lints in
    extract_result res
  | None -> Error { message = "failed to create janet string"; line = 0; column = 0 }
;;

let to_function (fd : funcdef) : Janet_function.t = F.janet_thunk fd
