type janet = [ `janet ] Ctypes.structure
type janet_function = [ `janet_function ] Ctypes.structure Ctypes_static.ptr
type janet_struct = [ `janet_struct_head ] Ctypes.structure Ctypes_static.ptr

type janet_cfunction =
  (int32 -> [ `janet ] Ctypes.structure Ctypes_static.ptr -> [ `janet ] Ctypes.structure)
    Ctypes_static.static_funptr

type janet_abstract = unit Ctypes_static.ptr
type janet_pointer = unit Ctypes_static.ptr
