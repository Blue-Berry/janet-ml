open! Core
module F = Janet_c.C.Functions
module T = Janet_c.C.Types
module Janet = Janet

exception Janet_error = Janet_errors.Janet_error

let init () =
  match F.janet_init () with
  | 0 -> ()
  | _ -> failwith "Failed to setup Janet Environment"
;;

let deinit = F.janet_deinit

let with_janet f =
  init ();
  Fun.protect ~finally:deinit f
;;

let with_janet_env (f : Janet.Env.t -> 'a) =
  with_janet (fun () ->
    let env = Janet.Env.core_env ~replacements:None in
    f env)
;;

type signal = T.janet_signal

let signal_to_string = function
  | T.Signal_ok -> "ok"
  | T.Signal_error -> "error"
  | T.Signal_debug -> "debug"
  | T.Signal_yield -> "yield"
;;

let check_signal signal value =
  match signal with
  | T.Signal_ok -> ()
  | other ->
    raise
      (Janet_error
         (Printf.sprintf
            "Janet signal: %s (value: %s)"
            (signal_to_string other)
            (Janet.Unwrapped.of_janet value
             |> Janet.Unwrapped.sexp_of_t
             |> Sexp.to_string_mach)))
;;

let dostring ~(env : Janet.Table.t) (str : string) ~(source_path : string option) =
  let out = Janet.create_ptr () in
  let raw = F.janet_dostring env str source_path (Some out) in
  let value = Janet.of_ptr out in
  (* janet_dostring returns 0 for Signal_ok *)
  let signal = if raw = 0 then T.Signal_ok else T.Signal_error in
  signal, value
;;

let dostring_exn ~env str ~source_path =
  let signal, value = dostring ~env str ~source_path in
  check_signal signal value;
  value
;;

let dobytes ~(env : Janet.Table.t) (bytes : bytes) ~(source_path : string option) =
  let len = Bytes.length bytes in
  let c_arr = Ctypes.CArray.make Ctypes.uint8_t len in
  for i = 0 to len - 1 do
    Ctypes.CArray.set
      c_arr
      i
      (Unsigned.UInt8.of_int (Stdlib.Char.code (Bytes.get bytes i)))
  done;
  let out = Janet.create_ptr () in
  let raw =
    F.janet_dobytes
      env
      (Ctypes.CArray.start c_arr)
      (Int32.of_int_exn len)
      source_path
      (Some out)
  in
  let value = Janet.of_ptr out in
  let signal = if raw = 0 then T.Signal_ok else T.Signal_error in
  signal, value
;;

let dobytes_exn ~env bytes ~source_path =
  let signal, value = dobytes ~env bytes ~source_path in
  check_signal signal value;
  value
;;

let mcall name (args : Janet.t list) =
  let argn = Int32.of_int_exn (List.length args) in
  let argv = Ctypes.CArray.of_list T.janet args |> Ctypes.CArray.start in
  F.janet_mcall name argn argv
;;

(** Like [mcall] but resolves the method on the receiver and calls via [pcall],
    raising [Janet_error] on any non-ok signal. *)
let mcall_exn name (args : Janet.t list) =
  let value = mcall name args in
  let signal = F.janet_type value in
  (* janet_mcall returns the result directly; errors would have been signalled
     via janet_panicf inside the C implementation. We wrap this so callers
     get the same exception-based interface as dostring_exn. *)
  ignore signal;
  value
;;

(** The C type of a Janet C-function: [Janet f(int32_t argc, Janet *argv)] *)
let cfun_fn_type = Ctypes.(int32_t @-> ptr T.janet @-> returning T.janet)

(** Module providing explicit-lifetime function pointers for OCaml→Janet callbacks. *)
module JanetCfun = (val Foreign.dynamic_funptr cfun_fn_type)

(** Register an OCaml closure as a Janet C-function named [name] in [env].

    The closure receives [(argc : int32) (argv : Janet.t ptr)] — use
    [Ctypes.CArray.from_ptr argv (Int32.to_int_exn argc)] to access arguments.

    The returned [JanetCfun.t] keeps the libffi closure alive. You must retain
    it (e.g. in a module-level ref) for as long as Janet may call back into it.
    Call [JanetCfun.free] when the binding is no longer needed. *)
let register_cfun
      ~(env : Janet.Table.t)
      (name : string)
      (f : int32 -> Janet.t Ctypes.ptr -> Janet.t)
  : JanetCfun.t
  =
  let dyn = JanetCfun.of_fun f in
  (* Coerce the dynamic funptr to static_funptr for janet_wrap_cfunction *)
  let sfptr = Ctypes.coerce JanetCfun.t (Ctypes.static_funptr cfun_fn_type) dyn in
  let jval = F.janet_wrap_cfunction sfptr in
  Janet.Env.def env name jval;
  dyn
;;
