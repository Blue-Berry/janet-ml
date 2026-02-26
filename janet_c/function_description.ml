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
  let janet_vm_s = Types.janet_vm
  let janet_funcdef_s = Types.janet_funcdef
  let janet_compile_result_s = Types.Janet_Compile_Result.t
  let janet_abstract_type_s = Types.janet_abstract_type
  let janet_parser_s = Types.Janet_Parser.t
  let janet_binding_type_enum = Types.janet_binding_type_enum

  (* Initialization *)
  let janet_init = foreign "janet_init" (void @-> returning int)
  let janet_deinit = foreign "janet_deinit" (void @-> returning void)

  (* VM management *)
  let janet_vm_alloc = foreign "janet_vm_alloc" (void @-> returning (ptr janet_vm_s))
  let janet_local_vm = foreign "janet_local_vm" (void @-> returning (ptr janet_vm_s))
  let janet_vm_free = foreign "janet_vm_free" (ptr janet_vm_s @-> returning void)
  let janet_vm_save = foreign "janet_vm_save" (ptr janet_vm_s @-> returning void)
  let janet_vm_load = foreign "janet_vm_load" (ptr janet_vm_s @-> returning void)

  let janet_interpreter_interrupt =
    foreign "janet_interpreter_interrupt" (ptr janet_vm_s @-> returning void)
  ;;

  let janet_interpreter_interrupt_handled =
    foreign "janet_interpreter_interrupt_handled" (ptr janet_vm_s @-> returning void)
  ;;

  (* Execution *)
  let janet_continue =
    foreign
      "janet_continue"
      (ptr janet_fiber_s @-> janet @-> ptr janet @-> returning Types.janet_signal_enum)
  ;;

  let janet_continue_signal =
    foreign
      "janet_continue_signal"
      (ptr janet_fiber_s
       @-> janet
       @-> ptr janet
       @-> Types.janet_signal_enum
       @-> returning Types.janet_signal_enum)
  ;;

  let janet_pcall =
    foreign
      "janet_pcall"
      (ptr janet_function_s
       @-> int32_t
       @-> ptr janet
       @-> ptr janet
       @-> ptr_opt (ptr janet_fiber_s)
       @-> returning Types.janet_signal_enum)
  ;;

  let janet_step =
    foreign
      "janet_step"
      (ptr janet_fiber_s @-> janet @-> ptr janet @-> returning Types.janet_signal_enum)
  ;;

  let janet_call =
    foreign
      "janet_call"
      (ptr janet_function_s @-> int32_t @-> ptr janet @-> returning janet)
  ;;

  let janet_mcall =
    foreign "janet_mcall" (string @-> int32_t @-> ptr janet @-> returning janet)
  ;;

  let janet_stacktrace =
    foreign "janet_stacktrace" (ptr janet_fiber_s @-> janet @-> returning void)
  ;;

  let janet_stacktrace_ext =
    foreign
      "janet_stacktrace_ext"
      (ptr janet_fiber_s @-> janet @-> const string @-> returning void)
  ;;

  (* Environment *)
  let janet_core_env =
    foreign "janet_core_env" (ptr_opt janet_table_s @-> returning (ptr janet_table_s))
  ;;

  (* Evaluation *)
  let janet_dostring =
    foreign
      "janet_dostring"
      (ptr janet_table_s
       @-> const string
       @-> string_opt
       @-> ptr_opt janet
       @-> returning int)
  ;;

  let janet_dobytes =
    foreign
      "janet_dobytes"
      (ptr janet_table_s
       @-> ptr (const uint8_t)
       @-> int32_t
       @-> string_opt
       @-> ptr_opt janet
       @-> returning int)
  ;;

  (* Type inspection *)
  let janet_type = foreign "janet_type" (janet @-> returning Types.janet_type_enum)

  let janet_checktype =
    foreign "janet_checktype" (janet @-> Types.janet_type_enum @-> returning int)
  ;;

  let janet_checktypes = foreign "janet_checktypes" (janet @-> int @-> returning int)
  let janet_truthy = foreign "janet_truthy" (janet @-> returning int)

  (* Wrap functions - create Janet values *)
  let janet_wrap_nil = foreign "janet_wrap_nil" (void @-> returning janet)
  let janet_wrap_number = foreign "janet_wrap_number" (double @-> returning janet)
  let janet_wrap_integer = foreign "janet_wrap_integer" (int32_t @-> returning janet)
  let janet_wrap_true = foreign "janet_wrap_true" (void @-> returning janet)
  let janet_wrap_false = foreign "janet_wrap_false" (void @-> returning janet)
  let janet_wrap_boolean = foreign "janet_wrap_boolean" (int @-> returning janet)

  let janet_wrap_string =
    foreign "janet_wrap_string" (ptr (const uint8_t) @-> returning janet)
  ;;

  let janet_wrap_symbol =
    foreign "janet_wrap_symbol" (ptr (const uint8_t) @-> returning janet)
  ;;

  let janet_wrap_keyword =
    foreign "janet_wrap_keyword" (ptr (const uint8_t) @-> returning janet)
  ;;

  let janet_wrap_table = foreign "janet_wrap_table" (ptr janet_table_s @-> returning janet)
  let janet_wrap_array = foreign "janet_wrap_array" (ptr janet_array_s @-> returning janet)

  let janet_wrap_buffer =
    foreign "janet_wrap_buffer" (ptr janet_buffer_s @-> returning janet)
  ;;

  let janet_wrap_tuple = foreign "janet_wrap_tuple" (ptr janet @-> returning janet)
  let janet_wrap_struct = foreign "janet_wrap_struct" (ptr janet_kv_s @-> returning janet)
  let janet_wrap_fiber = foreign "janet_wrap_fiber" (ptr janet_fiber_s @-> returning janet)

  let janet_wrap_function =
    foreign "janet_wrap_function" (ptr janet_function_s @-> returning janet)
  ;;

  let janet_wrap_cfunction =
    foreign
      "janet_wrap_cfunction"
      (static_funptr Ctypes.(int32_t @-> ptr janet @-> returning janet)
       @-> returning janet)
  ;;

  let janet_wrap_abstract = foreign "janet_wrap_abstract" (ptr void @-> returning janet)
  let janet_wrap_pointer = foreign "janet_wrap_pointer" (ptr void @-> returning janet)

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
  let janet_unwrap_tuple =
    foreign "janet_unwrap_tuple" (janet @-> returning (ptr (const janet)))
  ;;

  (* JanetStruct is const JanetKV * *)
  let janet_unwrap_struct =
    foreign "janet_unwrap_struct" (janet @-> returning (ptr (const janet_kv_s)))
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
  let janet_cstring =
    foreign "janet_cstring" (const string @-> returning (ptr_opt (const uint8_t)))
  ;;

  let janet_string =
    foreign
      "janet_string"
      (const (ptr uint8_t) @-> int32_t @-> returning (ptr_opt (const uint8_t)))
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
    foreign
      "janet_table_to_struct"
      (ptr janet_table_s @-> returning (ptr (const janet_kv_s)))
  ;;

  let janet_table_merge_table =
    foreign
      "janet_table_merge_table"
      (ptr janet_table_s @-> ptr janet_table_s @-> returning void)
  ;;

  let janet_table_merge_struct =
    foreign
      "janet_table_merge_struct"
      (ptr janet_table_s @-> ptr (const janet_kv_s) @-> returning void)
  ;;

  let janet_table_find =
    foreign "janet_table_find" (ptr janet_table_s @-> janet @-> returning (ptr janet_kv_s))
  ;;

  (* Tuple operations - JanetTuple is const Janet * *)
  let janet_tuple_begin = foreign "janet_tuple_begin" (int32_t @-> returning (ptr janet))

  let janet_tuple_end =
    foreign "janet_tuple_end" (ptr janet @-> returning (ptr (const janet)))
  ;;

  let janet_tuple_n =
    foreign
      "janet_tuple_n"
      (ptr (const janet) @-> int32_t @-> returning (ptr (const janet)))
  ;;

  (* Struct operations - JanetStruct is const JanetKV * *)
  let janet_struct_begin =
    foreign "janet_struct_begin" (int32_t @-> returning (ptr janet_kv_s))
  ;;

  let janet_struct_put =
    foreign "janet_struct_put" (ptr janet_kv_s @-> janet @-> janet @-> returning void)
  ;;

  let janet_struct_end =
    foreign "janet_struct_end" (ptr janet_kv_s @-> returning (ptr (const janet_kv_s)))
  ;;

  let janet_struct_get =
    foreign "janet_struct_get" (ptr (const janet_kv_s) @-> janet @-> returning janet)
  ;;

  let janet_struct_rawget =
    foreign "janet_struct_rawget" (ptr (const janet_kv_s) @-> janet @-> returning janet)
  ;;

  let janet_struct_to_table =
    foreign
      "janet_struct_to_table"
      (ptr (const janet_kv_s) @-> returning (ptr janet_table_s))
  ;;

  let janet_struct_find =
    foreign
      "janet_struct_find"
      (ptr (const janet_kv_s) @-> janet @-> returning (ptr (const janet_kv_s)))
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
      (ptr janet_buffer_s @-> ptr (const uint8_t) @-> int32_t @-> returning void)
  ;;

  let janet_buffer_push_string =
    foreign
      "janet_buffer_push_string"
      (ptr janet_buffer_s @-> ptr (const uint8_t) @-> returning void)
  ;;

  let janet_buffer_push_cstring =
    foreign
      "janet_buffer_push_cstring"
      (ptr janet_buffer_s @-> const string @-> returning void)
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

  (* Compilation *)
  let janet_compile =
    foreign
      "janet_compile"
      (janet
       @-> ptr janet_table_s
       @-> ptr (const uint8_t)
       @-> returning janet_compile_result_s)
  ;;

  let janet_compile_lint =
    foreign
      "janet_compile_lint"
      (janet
       @-> ptr janet_table_s
       @-> ptr (const uint8_t)
       @-> ptr janet_array_s
       @-> returning janet_compile_result_s)
  ;;

  let janet_thunk =
    foreign "janet_thunk" (ptr janet_funcdef_s @-> returning (ptr janet_function_s))
  ;;

  (* Abstract type allocation *)
  let janet_abstract =
    foreign
      "janet_abstract"
      (ptr janet_abstract_type_s @-> size_t @-> returning (ptr void))
  ;;

  (* Parser type descriptor *)
  let janet_parser_type = foreign_value "janet_parser_type" (const janet_abstract_type_s)

  (* Parser operations *)
  let janet_parser_init =
    foreign "janet_parser_init" (ptr janet_parser_s @-> returning void)
  ;;

  let janet_parser_deinit =
    foreign "janet_parser_deinit" (ptr janet_parser_s @-> returning void)
  ;;

  let janet_parser_consume =
    foreign "janet_parser_consume" (ptr janet_parser_s @-> uint8_t @-> returning void)
  ;;

  let janet_parser_status =
    foreign
      "janet_parser_status"
      (ptr janet_parser_s @-> returning Types.janet_parser_status_enum)
  ;;

  let janet_parser_produce =
    foreign "janet_parser_produce" (ptr janet_parser_s @-> returning janet)
  ;;

  let janet_parser_has_more =
    foreign "janet_parser_has_more" (ptr janet_parser_s @-> returning int)
  ;;

  let janet_parser_error =
    foreign "janet_parser_error" (ptr janet_parser_s @-> returning string_opt)
  ;;

  let janet_parser_eof = foreign "janet_parser_eof" (ptr janet_parser_s @-> returning void)

  let janet_parser_flush =
    foreign "janet_parser_flush" (ptr janet_parser_s @-> returning void)
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

  (* Marshalling *)
  let janet_marshal =
    foreign
      "janet_marshal"
      (ptr janet_buffer_s @-> janet @-> ptr_opt janet_table_s @-> int @-> returning void)
  ;;

  let janet_unmarshal =
    foreign
      "janet_unmarshal"
      (ptr (const uint8_t)
       @-> size_t
       @-> int
       @-> ptr_opt janet_table_s
       @-> ptr_opt (ptr (const uint8_t))
       @-> returning janet)
  ;;

  let janet_env_lookup =
    foreign "janet_env_lookup" (ptr janet_table_s @-> returning (ptr janet_table_s))
  ;;

  (* Arity checking *)
  let janet_arity =
    foreign "janet_arity" (int32_t @-> int32_t @-> int32_t @-> returning void)
  ;;

  let janet_fixarity = foreign "janet_fixarity" (int32_t @-> int32_t @-> returning void)

  (* Symbol/resolve *)
  let janet_csymbol =
    foreign "janet_csymbol" (const string @-> returning (ptr (const uint8_t)))
  ;;

  let janet_def =
    foreign
      "janet_def"
      (ptr janet_table_s @-> const string @-> janet @-> string_opt @-> returning void)
  ;;

  let janet_var =
    foreign
      "janet_var"
      (ptr janet_table_s @-> const string @-> janet @-> string_opt @-> returning void)
  ;;

  let janet_resolve =
    foreign
      "janet_resolve"
      (ptr janet_table_s
       @-> ptr (const uint8_t)
       @-> ptr janet
       @-> returning janet_binding_type_enum)
  ;;

  (* TODO: Remaining JANET_API functions to bind
   *
   * -- Abstract types --
   * janet_abstract_begin
   * janet_abstract_begin_threaded
   * janet_abstract_decref
   * janet_abstract_decref_maybe_free
   * janet_abstract_end
   * janet_abstract_end_threaded
   * janet_abstract_head
   * janet_abstract_incref
   * janet_abstract_threaded
   * janet_checkabstract
   * janet_get_abstract_type
   * janet_getabstract
   * janet_optabstract
   * janet_register_abstract_type
   *
   * -- Array --
   * janet_array_ensure
   * janet_array_n
   * janet_array_setcount
   * janet_array_weak
   *
   * -- Assembly --
   * janet_asm
   * janet_asm_decode_instruction
   * janet_disasm
   *
   * -- Async / Event loop --
   * janet_async_end
   * janet_async_in_flight
   * janet_async_start
   * janet_async_start_fiber
   * janet_await
   * janet_sleep_await
   * janet_addtimeout
   * janet_addtimeout_nil
   * janet_ev_dec_refcount
   * janet_ev_default_threaded_callback
   * janet_ev_inc_refcount
   * janet_ev_lasterr
   * janet_ev_post_event
   * janet_ev_read
   * janet_ev_readchunk
   * janet_ev_recv
   * janet_ev_recvchunk
   * janet_ev_recvfrom
   * janet_ev_send_buffer
   * janet_ev_send_string
   * janet_ev_sendto_buffer
   * janet_ev_sendto_string
   * janet_ev_threaded_await
   * janet_ev_threaded_call
   * janet_ev_write_buffer
   * janet_ev_write_string
   * janet_loop
   * janet_loop1
   * janet_loop1_interrupt
   * janet_loop_done
   * janet_loop_fiber
   * janet_schedule
   * janet_schedule_signal
   * janet_schedule_soon
   *
   * -- Atomic --
   * janet_atomic_dec
   * janet_atomic_inc
   * janet_atomic_load
   * janet_atomic_load_relaxed
   *
   * -- Cancel / Signal --
   * janet_cancel
   * janet_signalv
   *
   * -- C function registration --
   * janet_cfuns
   * janet_cfuns_ext
   * janet_cfuns_ext_prefix
   * janet_cfuns_prefix
   *
   * -- Channels --
   * janet_channel_give
   * janet_channel_make
   * janet_channel_make_threaded
   * janet_channel_take
   *
   * -- Type checking (get/opt from argv) --
   * janet_checkfile
   * janet_checkint
   * janet_checkint16
   * janet_checkint64
   * janet_checksize
   * janet_checkuint
   * janet_checkuint16
   * janet_checkuint64
   * janet_getargindex
   * janet_getarray
   * janet_getboolean
   * janet_getbuffer
   * janet_getbytes
   * janet_getcbytes
   * janet_getcfunction
   * janet_getchannel
   * janet_getcstring
   * janet_getdictionary
   * janet_getendrange
   * janet_getfiber
   * janet_getfile
   * janet_getflags
   * janet_getfunction
   * janet_gethalfrange
   * janet_getindex
   * janet_getindexed
   * janet_getinteger
   * janet_getinteger16
   * janet_getinteger64
   * janet_getjfile
   * janet_getkeyword
   * janet_getmethod
   * janet_getnat
   * janet_getnumber
   * janet_getpointer
   * janet_getsize
   * janet_getslice
   * janet_getstartrange
   * janet_getstring
   * janet_getstruct
   * janet_getsymbol
   * janet_gettable
   * janet_gettuple
   * janet_getuinteger
   * janet_getuinteger16
   * janet_getuinteger64
   * janet_optarray
   * janet_optboolean
   * janet_optbuffer
   * janet_optcbytes
   * janet_optcfunction
   * janet_optchannel
   * janet_optcstring
   * janet_optfiber
   * janet_optfunction
   * janet_optinteger
   * janet_optinteger64
   * janet_optkeyword
   * janet_optnat
   * janet_optnumber
   * janet_optpointer
   * janet_optsize
   * janet_optstring
   * janet_optstruct
   * janet_optsymbol
   * janet_opttable
   * janet_opttuple
   * janet_optuinteger
   * janet_optuinteger64
   *
   * -- Comparison / Hashing --
   * janet_compare
   * janet_hash
   * janet_keyeq
   * janet_streq
   * janet_symeq
   * janet_is_int
   *
   * -- Debug --
   * janet_debug_break
   * janet_debug_find
   * janet_debug_unbreak
   *
   * -- Dictionary (generic) --
   * janet_dictionary_get
   * janet_dictionary_next
   * janet_dictionary_view
   *
   * -- Dynamic bindings --
   * janet_dyn
   * janet_dynfile
   * janet_dynprintf
   * janet_setdyn
   *
   * -- Environment --
   * janet_core_lookup_table
   * janet_def_sm
   * janet_env_lookup_into
   * janet_register
   * janet_resolve_core
   * janet_var_sm
   *
   * -- Fiber --
   * janet_fiber_can_resume
   * janet_restore
   * janet_root_fiber
   *
   * -- File --
   * janet_file_close
   * janet_makefile
   * janet_makejfile
   * janet_unwrapfile
   *
   * -- Formatting --
   * janet_description
   * janet_description_b
   * janet_formatb
   * janet_formatbv
   * janet_formatc
   * janet_pretty
   * janet_to_string
   * janet_to_string_b
   *
   * -- Funcdef --
   * janet_funcdef_alloc
   * janet_thunk_delay
   * janet_verify
   *
   * -- GC --
   * janet_clear_memory
   * janet_gclock
   * janet_gcpressure
   * janet_gcunlock
   * janet_gcunrootall
   * janet_mark
   * janet_sweep
   *
   * -- Indexed (generic) --
   * janet_bytes_view
   * janet_in
   * janet_indexed_view
   * janet_lengthv
   * janet_next
   * janet_nextmethod
   * janet_putindex
   *
   * -- Marshal helpers --
   * janet_marshal_abstract
   * janet_marshal_byte
   * janet_marshal_bytes
   * janet_marshal_int
   * janet_marshal_int64
   * janet_marshal_janet
   * janet_marshal_ptr
   * janet_marshal_size
   * janet_unmarshal_abstract
   * janet_unmarshal_abstract_reuse
   * janet_unmarshal_abstract_threaded
   * janet_unmarshal_byte
   * janet_unmarshal_bytes
   * janet_unmarshal_ensure
   * janet_unmarshal_int
   * janet_unmarshal_int64
   * janet_unmarshal_janet
   * janet_unmarshal_ptr
   * janet_unmarshal_size
   *
   * -- Memory allocation --
   * janet_calloc
   * janet_free
   * janet_malloc
   * janet_realloc
   * janet_scalloc
   * janet_sfinalizer
   * janet_sfree
   * janet_smalloc
   * janet_srealloc
   *
   * -- Mutex / RWLock --
   * janet_os_mutex_deinit
   * janet_os_mutex_init
   * janet_os_mutex_lock
   * janet_os_mutex_size
   * janet_os_mutex_unlock
   * janet_os_rwlock_deinit
   * janet_os_rwlock_init
   * janet_os_rwlock_rlock
   * janet_os_rwlock_runlock
   * janet_os_rwlock_size
   * janet_os_rwlock_wlock
   * janet_os_rwlock_wunlock
   *
   * -- Nanbox internals --
   * janet_nanbox32_from_tagi
   * janet_nanbox32_from_tagp
   * janet_nanbox_from_bits
   * janet_nanbox_from_cpointer
   * janet_nanbox_from_double
   * janet_nanbox_from_pointer
   * janet_nanbox_to_pointer
   *
   * -- Native modules --
   * janet_native
   *
   * -- Panic --
   * janet_panic
   * janet_panic_abstract
   * janet_panic_type
   * janet_panicf
   * janet_panics
   * janet_panicv
   *
   * -- Parser --
   * janet_parser_produce_wrapped
   *
   * -- Pointer / Buffer --
   * janet_pointer_buffer_unsafe
   *
   * -- RNG --
   * janet_cryptorand
   * janet_default_rng
   * janet_rng_double
   * janet_rng_longseed
   * janet_rng_seed
   * janet_rng_u32
   *
   * -- Sandbox --
   * janet_sandbox
   * janet_sandbox_assert
   *
   * -- Scanning --
   * janet_scan_int64
   * janet_scan_number
   * janet_scan_number_base
   * janet_scan_numeric
   * janet_scan_uint64
   *
   * -- Sorted keys --
   * janet_sorted_keys
   *
   * -- Stream --
   * janet_cfun_stream_chunk
   * janet_cfun_stream_close
   * janet_cfun_stream_read
   * janet_cfun_stream_write
   * janet_stream
   * janet_stream_close
   * janet_stream_edge_triggered
   * janet_stream_ext
   * janet_stream_flags
   * janet_stream_level_triggered
   *
   * -- String --
   * janet_cstrcmp
   * janet_string_begin
   * janet_string_compare
   * janet_string_end
   * janet_string_equal
   * janet_string_equalconst
   * janet_string_head
   * janet_symbol
   * janet_symbol_gen
   *
   * -- Struct --
   * janet_struct_get_ex
   * janet_struct_head
   *
   * -- Table --
   * janet_table_clear
   * janet_table_clone
   * janet_table_deinit
   * janet_table_get_ex
   * janet_table_init
   * janet_table_init_raw
   * janet_table_weakk
   * janet_table_weakkv
   * janet_table_weakv
   *
   * -- Try --
   * janet_try_init
   *
   * -- Tuple --
   * janet_tuple_head
   *
   * -- Unwrap extras --
   * janet_unwrap_s64
   * janet_unwrap_u64
   *
   * -- Wrap extras --
   * janet_wrap_number_safe
   * janet_wrap_s64
   * janet_wrap_u64
   *
   * -- Hash key init --
   * janet_init_hash_key
   *)
end
