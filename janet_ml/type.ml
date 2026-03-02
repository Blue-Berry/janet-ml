type janet = [ `janet ] Ctypes.structure
type abstract = unit Ctypes_static.ptr
type buffer = [ `janet_buffer ] Ctypes.structure Ctypes_static.ptr

type cfunction =
  (int32 -> [ `janet ] Ctypes.structure Ctypes_static.ptr -> [ `janet ] Ctypes.structure)
    Ctypes_static.static_funptr

type funcdef = [ `janet_funcdef ] Ctypes.structure Ctypes_static.ptr
type compile_result = [ `janet_compile_result ] Ctypes.structure
type fiber = [ `janet_fiber ] Ctypes.structure Ctypes_static.ptr
type function_t = [ `janet_function ] Ctypes.structure Ctypes_static.ptr
type kv = [ `janet_kv ] Ctypes.structure Ctypes_static.ptr
type parser = [ `janet_parser ] Ctypes.structure Ctypes_static.ptr
type pointer = unit Ctypes_static.ptr
type struct_t = [ `janet_struct_head ] Ctypes.structure Ctypes_static.ptr
type table = [ `janet_table ] Ctypes.structure Ctypes_static.ptr
type tuple = [ `janet_tuple_head ] Ctypes.structure Ctypes_static.ptr
type vm = [ `janet_vm ] Ctypes.structure Ctypes_static.ptr
