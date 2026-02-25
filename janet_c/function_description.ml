open Ctypes
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let janet = Types.janet
  let janet_table_s = Types.janet_table
  let janet_array_s = Types.janet_array
  let janet_buffer_s = Types.janet_buffer
  let janet_fiber_s = Types.janet_fiber
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

  let janet_table_put =
    foreign "janet_table_put" (ptr janet_table_s @-> janet @-> janet @-> returning void)
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

  let janet_buffer_push_cstring =
    foreign "janet_buffer_push_cstring" (ptr janet_buffer_s @-> string @-> returning void)
  ;;

  let janet_buffer_push_u8 =
    foreign "janet_buffer_push_u8" (ptr janet_buffer_s @-> uint8_t @-> returning void)
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
