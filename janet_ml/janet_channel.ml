module Make (I : Janet_sig.S) = struct
  module F = Janet_c.C.Functions

  type t = Type.channel

  let make ?(limit = 0) () : t = F.janet_channel_make (Unsigned.UInt32.of_int limit)

  let make_threaded ?(limit = 0) () : t =
    F.janet_channel_make_threaded (Unsigned.UInt32.of_int limit)
  ;;

  let give (ch : t) (value : I.t) : bool = F.janet_channel_give ch value <> 0

  let take (ch : t) : I.t option =
    let out = I.create_ptr () in
    if F.janet_channel_take ch out <> 0 then Some (I.of_ptr out) else None
  ;;

  let get_from_argv (argv : I.ptr) (n : int) : t =
    F.janet_getchannel argv (Int32.of_int n)
  ;;

  let opt_from_argv (argv : I.ptr) ~(argc : int) (n : int) (default : t) : t =
    F.janet_optchannel argv (Int32.of_int argc) (Int32.of_int n) default
  ;;

  let to_janet (ch : t) : I.t = F.janet_wrap_abstract (Ctypes.to_voidp ch)
end
