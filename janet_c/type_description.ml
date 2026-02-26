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

  (* JanetKV - key/value pair used in structs *)
  let janet_kv : [ `janet_kv ] structure typ = structure "JanetKV"
  let janet_kv_key = field janet_kv "key" janet
  let janet_kv_value = field janet_kv "value" janet
  let () = seal janet_kv

  (* JanetGCObject - GC header embedded in all GC'd types *)
  let janet_gc_object : [ `janet_gc_object ] structure typ = structure "JanetGCObject"
  let _janet_gc_flags = field janet_gc_object "flags" int32_t
  let _janet_gc_data = field janet_gc_object "data" (ptr void)
  (* union { JanetGCObject *next; volatile int refcount; } *)

  let () = seal janet_gc_object

  (* Head structs for immutable GC types.
     The flexible array member "data" is not included in the ctypes struct;
     sizeof(head) equals offsetof(Head, data), used for pointer arithmetic. *)

  module Janet_Tuple = struct
    let head : [ `janet_tuple_head ] structure typ = structure "JanetTupleHead"
    let gc = field head "gc" janet_gc_object
    let length = field head "length" int32_t
    let hash = field head "hash" int32_t
    let sm_line = field head "sm_line" int32_t
    let sm_column = field head "sm_column" int32_t
    let () = seal head
  end

  module Janet_Struct = struct
    let head : [ `janet_struct_head ] structure typ = structure "JanetStructHead"
    let gc = field head "gc" janet_gc_object
    let length = field head "length" int32_t
    let hash = field head "hash" int32_t
    let capacity = field head "capacity" int32_t
    let proto = field head "proto" (ptr janet_kv)
    let () = seal head
  end

  module Janet_String = struct
    let head : [ `janet_string_head ] structure typ = structure "JanetStringHead"
    let gc = field head "gc" janet_gc_object
    let length = field head "length" int32_t
    let hash = field head "hash" int32_t
    let () = seal head
  end

  module Janet_Array = struct
    let t : [ `janet_array ] structure typ = structure "JanetArray"
    let gc = field t "gc" janet_gc_object
    let count = field t "count" int32_t
    let capacity = field t "capacity" int32_t
    let data = field t "data" (ptr janet)
    let () = seal t
  end

  module Janet_Buffer = struct
    let t : [ `janet_buffer ] structure typ = structure "JanetBuffer"
    let gc = field t "gc" janet_gc_object
    let count = field t "count" int32_t
    let capacity = field t "capacity" int32_t
    let data = field t "data" (ptr uint8_t)
    let () = seal t
  end

  (* JanetFunction - opaque, only used via pointer *)
  let janet_function : [ `janet_function ] structure typ = structure "JanetFunction"

  (* JanetVM - opaque, only used via pointer *)
  let janet_vm : [ `janet_vm ] structure typ = structure "JanetVM"

  (* JanetFuncDef - opaque, only used via pointer *)
  let janet_funcdef : [ `janet_funcdef ] structure typ = structure "JanetFuncDef"

  (* JanetAbstractType - opaque, only used via pointer *)
  let janet_abstract_type : [ `janet_abstract_type ] structure typ =
    structure "JanetAbstractType"
  ;;

  (* JanetParserStatus enum *)
  type janet_parser_status =
    | Parse_root
    | Parse_error
    | Parse_pending
    | Parse_dead

  let janet_parser_status_enum =
    enum
      "JanetParserStatus"
      ~typedef:false
      [ Parse_root, constant "JANET_PARSE_ROOT" int64_t
      ; Parse_error, constant "JANET_PARSE_ERROR" int64_t
      ; Parse_pending, constant "JANET_PARSE_PENDING" int64_t
      ; Parse_dead, constant "JANET_PARSE_DEAD" int64_t
      ]
  ;;

  (* JanetParser struct *)
  module Janet_Parser = struct
    let t : [ `janet_parser ] structure typ = structure "JanetParser"
    let args = field t "args" (ptr janet)
    let error = field t "error" string_opt
    let states = field t "states" (ptr void)
    let buf = field t "buf" (ptr uint8_t)
    let argcount = field t "argcount" size_t
    let argcap = field t "argcap" size_t
    let statecount = field t "statecount" size_t
    let statecap = field t "statecap" size_t
    let bufcount = field t "bufcount" size_t
    let bufcap = field t "bufcap" size_t
    let line = field t "line" size_t
    let column = field t "column" size_t
    let pending = field t "pending" size_t
    let lookback = field t "lookback" int
    let flag = field t "flag" int
    let () = seal t
  end

  type janet_fiber_status =
    | Status_dead
    | Status_error
    | Status_debug
    | Status_pending
    | Status_user0
    | Status_user1
    | Status_user2
    | Status_user3
    | Status_user4
    | Status_user5
    | Status_user6
    | Status_user7
    | Status_user8
    | Status_user9
    | Status_new
    | Status_alive

  let janet_fiber_status_enum =
    enum
      "JanetFiberStatus"
      ~typedef:true
      [ Status_dead, constant "JANET_STATUS_DEAD" int64_t
      ; Status_error, constant "JANET_STATUS_ERROR" int64_t
      ; Status_debug, constant "JANET_STATUS_DEBUG" int64_t
      ; Status_pending, constant "JANET_STATUS_PENDING" int64_t
      ; Status_user0, constant "JANET_STATUS_USER0" int64_t
      ; Status_user1, constant "JANET_STATUS_USER1" int64_t
      ; Status_user2, constant "JANET_STATUS_USER2" int64_t
      ; Status_user3, constant "JANET_STATUS_USER3" int64_t
      ; Status_user4, constant "JANET_STATUS_USER4" int64_t
      ; Status_user5, constant "JANET_STATUS_USER5" int64_t
      ; Status_user6, constant "JANET_STATUS_USER6" int64_t
      ; Status_user7, constant "JANET_STATUS_USER7" int64_t
      ; Status_user8, constant "JANET_STATUS_USER8" int64_t
      ; Status_user9, constant "JANET_STATUS_USER9" int64_t
      ; Status_new, constant "JANET_STATUS_NEW" int64_t
      ; Status_alive, constant "JANET_STATUS_ALIVE" int64_t
      ]
  ;;

  module Janet_Table = struct
    let t : [ `janet_table ] structure typ = structure "JanetTable"
    let gc = field t "gc" janet_gc_object
    let count = field t "count" int32_t
    let capacity = field t "capacity" int32_t
    let deleted = field t "deleted" int32_t
    let data = field t "data" (ptr janet_kv)
    let proto = field t "proto" (ptr_opt t)
    let () = seal t
  end

  module Janet_Fiber = struct
    let t : [ `janet_fiber ] structure typ = structure "JanetFiber"
    let gc = field t "gc" janet_gc_object
    let flags = field t "flags" int32_t
    let frame = field t "frame" int32_t
    let stackstart = field t "stackstart" int32_t
    let stacktop = field t "stacktop" int32_t
    let capacity = field t "capacity" int32_t
    let maxstack = field t "maxstack" int32_t

    (* env is JanetTable* but JanetTable is not yet defined;
       use ptr void to break the circular dependency *)
    let env = field t "env" (ptr Janet_Table.t)
    let data = field t "data" (ptr janet)
    let child = field t "child" (ptr_opt t)
    let last_value = field t "last_value" janet
    let () = seal t
  end

  (* JanetSourceMapping *)
  let janet_source_mapping : [ `janet_source_mapping ] structure typ =
    structure "JanetSourceMapping"
  ;;

  let janet_source_mapping_line = field janet_source_mapping "line" int32_t
  let janet_source_mapping_column = field janet_source_mapping "column" int32_t
  let () = seal janet_source_mapping

  (* JanetCompileStatus enum *)
  type janet_compile_status =
    | Compile_ok
    | Compile_error

  let janet_compile_status_enum =
    enum
      "JanetCompileStatus"
      ~typedef:false
      [ Compile_ok, constant "JANET_COMPILE_OK" int64_t
      ; Compile_error, constant "JANET_COMPILE_ERROR" int64_t
      ]
  ;;

  (* JanetCompileResult *)
  module Janet_Compile_Result = struct
    let t : [ `janet_compile_result ] structure typ = structure "JanetCompileResult"
    let funcdef = field t "funcdef" (ptr janet_funcdef)
    let error = field t "error" (ptr uint8_t)
    let macrofiber = field t "macrofiber" (ptr Janet_Fiber.t)
    let error_mapping = field t "error_mapping" janet_source_mapping
    let status = field t "status" janet_compile_status_enum
    let () = seal t
  end

  (* JanetBindingType enum *)
  type janet_binding_type =
    | Binding_none
    | Binding_def
    | Binding_var
    | Binding_macro
    | Binding_dynamic_def
    | Binding_dynamic_macro

  let janet_binding_type_enum =
    enum
      "JanetBindingType"
      ~typedef:true
      [ Binding_none, constant "JANET_BINDING_NONE" int64_t
      ; Binding_def, constant "JANET_BINDING_DEF" int64_t
      ; Binding_var, constant "JANET_BINDING_VAR" int64_t
      ; Binding_macro, constant "JANET_BINDING_MACRO" int64_t
      ; Binding_dynamic_def, constant "JANET_BINDING_DYNAMIC_DEF" int64_t
      ; Binding_dynamic_macro, constant "JANET_BINDING_DYNAMIC_MACRO" int64_t
      ]
  ;;
end
