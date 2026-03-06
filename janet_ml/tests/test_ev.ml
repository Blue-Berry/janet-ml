(** Tests for the Janet event loop bindings (Janet.Ev). *)

open! Core
open Janet_ml
open Janet_ml.Janet

let lookup_fn env name =
  match Env.Binding.lookup ~env name |> Env.Binding.to_janet |> Unwrapped.of_janet with
  | Unwrapped.Function f -> f
  | _ -> failwith (Printf.sprintf "'%s' is not a function" name)
;;

let%expect_test "is_done returns true when no fibers are scheduled" =
  with_janet_env (fun _env -> Printf.printf "done=%b\n" (Ev.is_done ()));
  [%expect {| done=true |}]
;;

let%expect_test "schedule + run executes a fiber to completion" =
  with_janet_env (fun env ->
    let _ = dostring_exn ~env "(defn give-42 [] 42)" in
    let fiber = Fiber.create (lookup_fn env "give-42") ~capacity:64 ~argv:[] in
    Ev.schedule fiber (create ());
    Ev.run ();
    let status = Fiber.status fiber in
    match status with
    | Fiber.Dead -> print_endline "Dead"
    | s -> Fiber.sexp_of_status s |> Sexp.to_string_hum |> print_endline);
  [%expect {| Dead |}]
;;

let%expect_test "loop_fiber runs a fiber to completion" =
  with_janet_env (fun env ->
    let _ = dostring_exn ~env "(defn add-one [] (+ 1 2))" in
    let fiber = Fiber.create (lookup_fn env "add-one") ~capacity:64 ~argv:[] in
    let rc = Ev.loop_fiber fiber in
    let status = Fiber.status fiber in
    Printf.printf
      "rc=%d status=%s\n"
      rc
      (match status with
       | Fiber.Dead -> "Dead"
       | s -> Fiber.sexp_of_status s |> Sexp.to_string_hum));
  [%expect {| rc=0 status=Dead |}]
;;

let%expect_test "step-based custom loop drives fibers" =
  with_janet_env (fun env ->
    let _ = dostring_exn ~env "(defn compute [] (+ 40 59))" in
    let fiber = Fiber.create (lookup_fn env "compute") ~capacity:64 ~argv:[] in
    Ev.schedule fiber (create ());
    let turns = ref 0 in
    while not (Ev.is_done ()) do
      let _ = Ev.step () in
      incr turns
    done;
    let status = Fiber.status fiber in
    Printf.printf
      "status=%s turns>0=%b\n"
      (match status with
       | Fiber.Dead -> "Dead"
       | s -> Fiber.sexp_of_status s |> Sexp.to_string_hum)
      (!turns > 0));
  [%expect {| status=Dead turns>0=true |}]
;;
