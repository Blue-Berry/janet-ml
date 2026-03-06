module Make (I : Janet_sig.S) = struct
  module F = Janet_c.C.Functions
  module Fiber = Janet_fiber.Make (I)

  let run = F.janet_loop
  let is_done () = F.janet_loop_done () <> 0
  let step () : Fiber.t option = F.janet_loop1 ()
  let interrupt (vm : Type.vm) = F.janet_loop1_interrupt vm
  let schedule (fiber : Fiber.t) (value : I.t) = F.janet_schedule fiber value

  let schedule_signal (fiber : Fiber.t) (value : I.t) (signal : Fiber.signal) =
    F.janet_schedule_signal fiber value signal
  ;;

  let schedule_soon (fiber : Fiber.t) (value : I.t) (signal : Fiber.signal) =
    F.janet_schedule_soon fiber value signal
  ;;

  let loop_fiber (fiber : Fiber.t) : int = F.janet_loop_fiber fiber
end
