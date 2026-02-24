open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  (* Version constants *)
  let janet_version_major = constant "JANET_VERSION_MAJOR" int
  let janet_version_minor = constant "JANET_VERSION_MINOR" int
  let janet_version_patch = constant "JANET_VERSION_PATCH" int

  (* JanetType enum *)
  type janet_type =
    | Number
    | Nil
    | Boolean
    | Fiber
    | String
    | Symbol
    | Keyword
    | Array
    | Tuple
    | Table
    | Struct
    | Buffer
    | Function
    | CFunction
    | Abstract
    | Pointer

  let janet_type_enum =
    enum
      "JanetType"
      ~typedef:true
      [ Number, constant "JANET_NUMBER" int64_t
      ; Nil, constant "JANET_NIL" int64_t
      ; Boolean, constant "JANET_BOOLEAN" int64_t
      ; Fiber, constant "JANET_FIBER" int64_t
      ; String, constant "JANET_STRING" int64_t
      ; Symbol, constant "JANET_SYMBOL" int64_t
      ; Keyword, constant "JANET_KEYWORD" int64_t
      ; Array, constant "JANET_ARRAY" int64_t
      ; Tuple, constant "JANET_TUPLE" int64_t
      ; Table, constant "JANET_TABLE" int64_t
      ; Struct, constant "JANET_STRUCT" int64_t
      ; Buffer, constant "JANET_BUFFER" int64_t
      ; Function, constant "JANET_FUNCTION" int64_t
      ; CFunction, constant "JANET_CFUNCTION" int64_t
      ; Abstract, constant "JANET_ABSTRACT" int64_t
      ; Pointer, constant "JANET_POINTER" int64_t
      ]
  ;;

  (* JanetSignal enum *)
  type janet_signal =
    | Signal_ok
    | Signal_error
    | Signal_debug
    | Signal_yield

  let janet_signal_enum =
    enum
      "JanetSignal"
      ~typedef:true
      [ Signal_ok, constant "JANET_SIGNAL_OK" int64_t
      ; Signal_error, constant "JANET_SIGNAL_ERROR" int64_t
      ; Signal_debug, constant "JANET_SIGNAL_DEBUG" int64_t
      ; Signal_yield, constant "JANET_SIGNAL_YIELD" int64_t
      ]
  ;;

  (* Janet value - non-nanbox struct layout:
     struct Janet {
       union { uint64_t u64; double number; int32_t integer;
               void *pointer; const void *cpointer; } as;
       JanetType type;
     };
     The inner union is anonymous in C so we represent it as uint64_t
     (all union members fit in 8 bytes). Actual field access goes through
     the wrap/unwrap API functions. *)
  let janet : [ `janet ] structure typ = structure "Janet"
  let janet_as = field janet "as" uint64_t
  let janet_type_field = field janet "type" int
  let () = seal janet

  (* Opaque GC'd types - only used via pointer *)
  let janet_table : [ `janet_table ] structure typ = structure "JanetTable"
  let janet_array : [ `janet_array ] structure typ = structure "JanetArray"
  let janet_buffer : [ `janet_buffer ] structure typ = structure "JanetBuffer"
  let janet_fiber : [ `janet_fiber ] structure typ = structure "JanetFiber"
  let janet_function : [ `janet_function ] structure typ = structure "JanetFunction"
end
