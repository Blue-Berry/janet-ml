open Ctypes
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let janet = Types.janet
  let janet_kv_s = Types.janet_kv
  let janet_table_s = Types.Janet_Table.t
  let janet_array_s = Types.Janet_Array.t
  let janet_buffer_s = Types.Janet_Buffer.t
  let janet_fiber_s = Types.Janet_Fiber.t
  let janet_function_s = Types.janet_function

  (* Initialization *)
  let janet_init = foreign "janet_init" (void @-> returning int)
  let janet_deinit = foreign "janet_deinit" (void @-> returning void)

  (* Environment *)
  let janet_core_env =
    foreign "janet_core_env" (ptr_opt janet_table_s @-> returning (ptr janet_table_s))
  ;;

  (* Evaluation *)
  let janet_dostring =
    foreign
      "janet_dostring"
      (ptr janet_table_s @-> string @-> string_opt @-> ptr_opt janet @-> returning int)
  ;;

  let janet_dobytes =
    foreign
      "janet_dobytes"
      (ptr janet_table_s
       @-> ptr uint8_t
       @-> int32_t
       @-> string_opt
       @-> ptr_opt janet
       @-> returning int)
  ;;

  (* Type inspection *)
  let janet_type = foreign "janet_type" (janet @-> returning Types.janet_type_enum)

  (* Wrap functions - create Janet values *)
  let janet_wrap_nil = foreign "janet_wrap_nil" (void @-> returning janet)
  let janet_wrap_number = foreign "janet_wrap_number" (double @-> returning janet)
  let janet_wrap_integer = foreign "janet_wrap_integer" (int32_t @-> returning janet)
  let janet_wrap_boolean = foreign "janet_wrap_boolean" (int @-> returning janet)
  let janet_wrap_string = foreign "janet_wrap_string" (ptr uint8_t @-> returning janet)
  let janet_wrap_table = foreign "janet_wrap_table" (ptr janet_table_s @-> returning janet)
  let janet_wrap_array = foreign "janet_wrap_array" (ptr janet_array_s @-> returning janet)

  let janet_wrap_buffer =
    foreign "janet_wrap_buffer" (ptr janet_buffer_s @-> returning janet)
  ;;

  let janet_wrap_struct = foreign "janet_wrap_struct" (ptr janet_kv_s @-> returning janet)
  let janet_wrap_fiber = foreign "janet_wrap_fiber" (ptr janet_fiber_s @-> returning janet)

  let janet_wrap_function =
    foreign "janet_wrap_function" (ptr janet_function_s @-> returning janet)
  ;;

  (* Unwrap functions - extract values from Janet *)
  let janet_unwrap_number = foreign "janet_unwrap_number" (janet @-> returning double)
  let janet_unwrap_integer = foreign "janet_unwrap_integer" (janet @-> returning int32_t)
  let janet_unwrap_boolean = foreign "janet_unwrap_boolean" (janet @-> returning int)

  let janet_unwrap_table =
    foreign "janet_unwrap_table" (janet @-> returning (ptr janet_table_s))
  ;;

  let janet_unwrap_array =
    foreign "janet_unwrap_array" (janet @-> returning (ptr janet_array_s))
  ;;

  let janet_unwrap_buffer =
    foreign "janet_unwrap_buffer" (janet @-> returning (ptr janet_buffer_s))
  ;;

  let janet_unwrap_fiber =
    foreign "janet_unwrap_fiber" (janet @-> returning (ptr janet_fiber_s))
  ;;

  let janet_unwrap_function =
    foreign "janet_unwrap_function" (janet @-> returning (ptr janet_function_s))
  ;;

  (* JanetString/JanetSymbol/JanetKeyword are all const uint8_t * *)
  let janet_unwrap_string = foreign "janet_unwrap_string" (janet @-> returning string)
  let janet_unwrap_symbol = foreign "janet_unwrap_symbol" (janet @-> returning string)
  let janet_unwrap_keyword = foreign "janet_unwrap_keyword" (janet @-> returning string)

  (* JanetTuple is const Janet * *)
  let janet_unwrap_tuple = foreign "janet_unwrap_tuple" (janet @-> returning (ptr janet))

  (* JanetStruct is const JanetKV * *)
  let janet_unwrap_struct =
    foreign "janet_unwrap_struct" (janet @-> returning (ptr janet_kv_s))
  ;;

  (* JanetAbstract is void *, void *janet_unwrap_pointer *)
  let janet_unwrap_abstract =
    foreign "janet_unwrap_abstract" (janet @-> returning (ptr void))
  ;;

  let janet_unwrap_pointer =
    foreign "janet_unwrap_pointer" (janet @-> returning (ptr void))
  ;;

  (* JanetCFunction is Janet(*)(int32_t, Janet*) *)
  let janet_unwrap_cfunction =
    foreign
      "janet_unwrap_cfunction"
      (janet
       @-> returning (static_funptr Ctypes.(int32_t @-> ptr janet @-> returning janet)))
  ;;

  (* String operations - JanetString is const uint8_t* *)
  let janet_cstring = foreign "janet_cstring" (string @-> returning (ptr_opt uint8_t))

  let janet_string =
    foreign "janet_string" (ptr uint8_t @-> int32_t @-> returning (ptr_opt uint8_t))
  ;;

  (* Table operations *)
  let janet_table = foreign "janet_table" (int32_t @-> returning (ptr janet_table_s))

  let janet_table_get =
    foreign "janet_table_get" (ptr janet_table_s @-> janet @-> returning janet)
  ;;

  let janet_table_rawget =
    foreign "janet_table_rawget" (ptr janet_table_s @-> janet @-> returning janet)
  ;;

  let janet_table_remove =
    foreign "janet_table_remove" (ptr janet_table_s @-> janet @-> returning janet)
  ;;

  let janet_table_put =
    foreign "janet_table_put" (ptr janet_table_s @-> janet @-> janet @-> returning void)
  ;;

  let janet_table_to_struct =
    foreign "janet_table_to_struct" (ptr janet_table_s @-> returning (ptr janet_kv_s))
  ;;

  let janet_table_merge_table =
    foreign
      "janet_table_merge_table"
      (ptr janet_table_s @-> ptr janet_table_s @-> returning void)
  ;;

  let janet_table_merge_struct =
    foreign
      "janet_table_merge_struct"
      (ptr janet_table_s @-> ptr janet_kv_s @-> returning void)
  ;;

  let janet_table_find =
    foreign "janet_table_find" (ptr janet_table_s @-> janet @-> returning (ptr janet_kv_s))
  ;;

  (* Struct operations - JanetStruct is const JanetKV * *)
  let janet_struct_begin =
    foreign "janet_struct_begin" (int32_t @-> returning (ptr janet_kv_s))
  ;;

  let janet_struct_put =
    foreign "janet_struct_put" (ptr janet_kv_s @-> janet @-> janet @-> returning void)
  ;;

  let janet_struct_end =
    foreign "janet_struct_end" (ptr janet_kv_s @-> returning (ptr janet_kv_s))
  ;;

  let janet_struct_get =
    foreign "janet_struct_get" (ptr janet_kv_s @-> janet @-> returning janet)
  ;;

  let janet_struct_rawget =
    foreign "janet_struct_rawget" (ptr janet_kv_s @-> janet @-> returning janet)
  ;;

  let janet_struct_to_table =
    foreign "janet_struct_to_table" (ptr janet_kv_s @-> returning (ptr janet_table_s))
  ;;

  let janet_struct_find =
    foreign "janet_struct_find" (ptr janet_kv_s @-> janet @-> returning (ptr janet_kv_s))
  ;;

  (* Array operations *)
  let janet_array = foreign "janet_array" (int32_t @-> returning (ptr janet_array_s))

  let janet_array_push =
    foreign "janet_array_push" (ptr janet_array_s @-> janet @-> returning void)
  ;;

  let janet_array_pop = foreign "janet_array_pop" (ptr janet_array_s @-> returning janet)
  let janet_array_peek = foreign "janet_array_peek" (ptr janet_array_s @-> returning janet)

  (* Buffer operations *)
  let janet_buffer = foreign "janet_buffer" (int32_t @-> returning (ptr janet_buffer_s))

  let janet_buffer_init =
    foreign
      "janet_buffer_init"
      (ptr janet_buffer_s @-> int32_t @-> returning (ptr janet_buffer_s))
  ;;

  let janet_buffer_deinit =
    foreign "janet_buffer_deinit" (ptr janet_buffer_s @-> returning void)
  ;;

  let janet_buffer_ensure =
    foreign
      "janet_buffer_ensure"
      (ptr janet_buffer_s @-> int32_t @-> int32_t @-> returning void)
  ;;

  let janet_buffer_setcount =
    foreign "janet_buffer_setcount" (ptr janet_buffer_s @-> int32_t @-> returning void)
  ;;

  let janet_buffer_extra =
    foreign "janet_buffer_extra" (ptr janet_buffer_s @-> int32_t @-> returning void)
  ;;

  let janet_buffer_push_bytes =
    foreign
      "janet_buffer_push_bytes"
      (ptr janet_buffer_s @-> ptr uint8_t @-> int32_t @-> returning void)
  ;;

  let janet_buffer_push_string =
    foreign
      "janet_buffer_push_string"
      (ptr janet_buffer_s @-> ptr uint8_t @-> returning void)
  ;;

  let janet_buffer_push_cstring =
    foreign "janet_buffer_push_cstring" (ptr janet_buffer_s @-> string @-> returning void)
  ;;

  let janet_buffer_push_u8 =
    foreign "janet_buffer_push_u8" (ptr janet_buffer_s @-> uint8_t @-> returning void)
  ;;

  let janet_buffer_push_u16 =
    foreign "janet_buffer_push_u16" (ptr janet_buffer_s @-> uint16_t @-> returning void)
  ;;

  let janet_buffer_push_u32 =
    foreign "janet_buffer_push_u32" (ptr janet_buffer_s @-> uint32_t @-> returning void)
  ;;

  let janet_buffer_push_u64 =
    foreign "janet_buffer_push_u64" (ptr janet_buffer_s @-> uint64_t @-> returning void)
  ;;

  (* Fiber operations *)
  let janet_fiber =
    foreign
      "janet_fiber"
      (ptr janet_function_s
       @-> int32_t
       @-> int32_t
       @-> ptr janet
       @-> returning (ptr janet_fiber_s))
  ;;

  let janet_fiber_reset =
    foreign
      "janet_fiber_reset"
      (ptr janet_fiber_s
       @-> ptr janet_function_s
       @-> int32_t
       @-> ptr janet
       @-> returning (ptr janet_fiber_s))
  ;;

  let janet_fiber_status =
    foreign
      "janet_fiber_status"
      (ptr janet_fiber_s @-> returning Types.janet_fiber_status_enum)
  ;;

  let janet_current_fiber =
    foreign "janet_current_fiber" (void @-> returning (ptr janet_fiber_s))
  ;;

  (* GC *)
  let janet_collect = foreign "janet_collect" (void @-> returning void)
  let janet_gcroot = foreign "janet_gcroot" (janet @-> returning void)
  let janet_gcunroot = foreign "janet_gcunroot" (janet @-> returning int)

  (* Comparison and utility *)
  let janet_equals = foreign "janet_equals" (janet @-> janet @-> returning int)
  let janet_length = foreign "janet_length" (janet @-> returning int32_t)
  let janet_get = foreign "janet_get" (janet @-> janet @-> returning janet)
  let janet_put = foreign "janet_put" (janet @-> janet @-> janet @-> returning void)
end
