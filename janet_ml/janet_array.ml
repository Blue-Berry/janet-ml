module Make (I : Janet_sig.S) = struct
  module F = Janet_c.C.Functions
  module T = Janet_c.C.Types

  type t = Type.array

  let create capacity : t = F.janet_array (Int32.of_int capacity)
  let push (arr : t) (value : I.t) = F.janet_array_push arr value
  let pop (arr : t) : I.t = F.janet_array_pop arr
  let peek (arr : t) : I.t = F.janet_array_peek arr
  let count (arr : t) = Ctypes.getf Ctypes.(!@arr) T.Janet_Array.count |> Int32.to_int

  let capacity (arr : t) =
    Ctypes.getf Ctypes.(!@arr) T.Janet_Array.capacity |> Int32.to_int
  ;;

  let get (arr : t) (i : int) : I.t =
    let data = Ctypes.getf Ctypes.(!@arr) T.Janet_Array.data in
    Ctypes.(!@(data +@ i))
  ;;

  let set (arr : t) (i : int) (value : I.t) =
    let data = Ctypes.getf Ctypes.(!@arr) T.Janet_Array.data in
    Ctypes.(data +@ i <-@ value)
  ;;

  let to_list (arr : t) : I.t list =
    let n = count arr in
    let data = Ctypes.getf Ctypes.(!@arr) T.Janet_Array.data in
    Ctypes.CArray.from_ptr data n |> Ctypes.CArray.to_list
  ;;

  let to_array (arr : t) : I.t array = to_list arr |> Array.of_list

  let of_list (values : I.t list) : t =
    let n = List.length values in
    let arr = create n in
    List.iter (fun v -> push arr v) values;
    arr
  ;;

  let of_array (values : I.t array) : t = of_list (Array.to_list values)
  let wrap (arr : t) : I.t = F.janet_wrap_array arr
  let unwrap (j : I.t) : t = F.janet_unwrap_array j
end
