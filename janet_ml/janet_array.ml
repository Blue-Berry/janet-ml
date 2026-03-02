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

  (** Ensure the array has at least [capacity] slots, growing by [growth] factor
      if a reallocation is needed. *)
  let ensure (arr : t) ~(capacity : int) ~(growth : int) : unit =
    F.janet_array_ensure arr (Int32.of_int capacity) (Int32.of_int growth)
  ;;

  (** Set the logical count of the array, truncating or zero-padding as needed. *)
  let set_count (arr : t) (n : int) : unit =
    F.janet_array_setcount arr (Int32.of_int n)
  ;;

  (** Create a Janet array from an OCaml array of Janet values, using the C
      [janet_array_n] constructor (single allocation). *)
  let of_janet_array (values : I.t array) : t =
    let n = Array.length values in
    let c_arr = Ctypes.CArray.of_list T.janet (Array.to_list values) in
    F.janet_array_n (Ctypes.CArray.start c_arr) (Int32.of_int n)
  ;;
end
