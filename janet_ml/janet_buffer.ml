module F = Janet_c.C.Functions
module T = Janet_c.C.Types

type t = [ `janet_buffer ] Ctypes.structure Ctypes_static.ptr

let create capacity : t = F.janet_buffer (Int32.of_int capacity)
let deinit (buf : t) = F.janet_buffer_deinit buf

let ensure (buf : t) ~capacity ~growth =
  F.janet_buffer_ensure buf (Int32.of_int capacity) (Int32.of_int growth)
;;

let set_count (buf : t) count = F.janet_buffer_setcount buf (Int32.of_int count)
let extra (buf : t) n = F.janet_buffer_extra buf (Int32.of_int n)

let push_bytes (buf : t) (bytes : bytes) =
  let len = Bytes.length bytes in
  let c_arr = Ctypes.CArray.make Ctypes.uint8_t len in
  for i = 0 to len - 1 do
    Ctypes.CArray.set c_arr i (Unsigned.UInt8.of_int (Char.code (Bytes.get bytes i)))
  done;
  F.janet_buffer_push_bytes buf (Ctypes.CArray.start c_arr) (Int32.of_int len)
;;

let push_string (buf : t) (s : string) = F.janet_buffer_push_cstring buf s
let push_u8 (buf : t) (x : int) = F.janet_buffer_push_u8 buf (Unsigned.UInt8.of_int x)
let push_u16 (buf : t) (x : int) = F.janet_buffer_push_u16 buf (Unsigned.UInt16.of_int x)
let push_u32 (buf : t) (x : int) = F.janet_buffer_push_u32 buf (Unsigned.UInt32.of_int x)
let push_u64 (buf : t) (x : int) = F.janet_buffer_push_u64 buf (Unsigned.UInt64.of_int x)
let count (buf : t) = Ctypes.getf Ctypes.(!@buf) T.Janet_Buffer.count |> Int32.to_int

let capacity (buf : t) =
  Ctypes.getf Ctypes.(!@buf) T.Janet_Buffer.capacity |> Int32.to_int
;;

let contents (buf : t) : bytes =
  let n = count buf in
  let data = Ctypes.getf Ctypes.(!@buf) T.Janet_Buffer.data in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set b i (Char.chr (Unsigned.UInt8.to_int Ctypes.(!@(data +@ i))))
  done;
  b
;;

let to_string (buf : t) : string = contents buf |> Bytes.to_string
let wrap (buf : t) : Janet_types.janet = F.janet_wrap_buffer buf
let unwrap (j : Janet_types.janet) : t = F.janet_unwrap_buffer j
