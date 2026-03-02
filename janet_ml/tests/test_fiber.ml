(** Tests for Fiber.cancel, Fiber.can_resume, and fiber lifecycle. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let lookup_fn env name =
  match Env.Binding.lookup ~env name |> Env.Binding.to_janet |> Unwrapped.of_janet with
  | Unwrapped.Function f -> f
  | _ -> failwith (Printf.sprintf "'%s' is not a function" name)
;;

let signal_name = function
  | Janet_c.C.Types.Signal_ok -> "ok"
  | Janet_c.C.Types.Signal_error -> "error"
  | Janet_c.C.Types.Signal_debug -> "debug"
  | Janet_c.C.Types.Signal_yield -> "yield"
;;

let%expect_test "a freshly created fiber has status New" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn noop [] nil)" ~source_path:None in
    let fiber = Fiber.create (lookup_fn env "noop") ~capacity:64 ~argv:[] in
    match Fiber.status fiber with
    | Fiber.New -> print_endline "New"
    | s -> Fiber.sexp_of_status s |> Sexp.to_string_hum |> print_endline);
  [%expect {| New |}]
;;

let%expect_test "running a fiber to completion produces status Dead" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn give-42 [] 42)" ~source_path:None in
    let fiber = Fiber.create (lookup_fn env "give-42") ~capacity:64 ~argv:[] in
    let sig_, result = Fiber.continue fiber (create ()) in
    Printf.printf
      "signal=%s result=%s\n"
      (signal_name sig_)
      (result |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> Sexp.to_string_hum);
    match Fiber.status fiber with
    | Fiber.Dead -> print_endline "Dead"
    | s -> Fiber.sexp_of_status s |> Sexp.to_string_hum |> print_endline);
  [%expect
    {|
    signal=ok result=(Number 42)
    Dead
    |}]
;;

let%expect_test "a yielding fiber produces Signal_yield and status Pending" =
  with_janet_env (fun env ->
    let _ =
      Janet_ml.dostring_exn ~env "(defn gen [] (yield 1) (yield 2))" ~source_path:None
    in
    let fiber = Fiber.create (lookup_fn env "gen") ~capacity:64 ~argv:[] in
    let sig1, v1 = Fiber.continue fiber (create ()) in
    Printf.printf
      "%s %s\n"
      (signal_name sig1)
      (v1 |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> Sexp.to_string_hum);
    match Fiber.status fiber with
    | Fiber.Pending -> print_endline "Pending"
    | s -> Fiber.sexp_of_status s |> Sexp.to_string_hum |> print_endline);
  [%expect
    {|
    yield (Number 1)
    Pending
    |}]
;;

let%expect_test "can_resume returns true for a Pending fiber" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn gen [] (yield 1) 2)" ~source_path:None in
    let fiber = Fiber.create (lookup_fn env "gen") ~capacity:64 ~argv:[] in
    let _ = Fiber.continue fiber (create ()) in
    Printf.printf "can_resume=%b\n" (Fiber.can_resume fiber));
  [%expect {| can_resume=true |}]
;;

let%expect_test "can_resume returns false for a Dead fiber" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn done [] :done)" ~source_path:None in
    let fiber = Fiber.create (lookup_fn env "done") ~capacity:64 ~argv:[] in
    let _ = Fiber.continue fiber (create ()) in
    Printf.printf "can_resume=%b\n" (Fiber.can_resume fiber));
  [%expect {| can_resume=false |}]
;;

let%expect_test "Fiber.cancel puts a pending fiber into Error state" =
  with_janet_env (fun env ->
    let _ =
      Janet_ml.dostring_exn
        ~env
        "(defn looper [] (forever (yield :tick)))"
        ~source_path:None
    in
    let fiber = Fiber.create (lookup_fn env "looper") ~capacity:64 ~argv:[] in
    let _ = Fiber.continue fiber (create ()) in
    Fiber.cancel fiber (Unwrapped.to_janet (Unwrapped.String "timed out"));
    match Fiber.status fiber with
    | Fiber.Error -> print_endline "Error"
    | s -> Fiber.sexp_of_status s |> Sexp.to_string_hum |> print_endline);
  [%expect {| Error |}]
;;

let%expect_test "Fiber.cancel makes can_resume return false" =
  with_janet_env (fun env ->
    let _ =
      Janet_ml.dostring_exn ~env "(defn gen [] (yield 1) (yield 2))" ~source_path:None
    in
    let fiber = Fiber.create (lookup_fn env "gen") ~capacity:64 ~argv:[] in
    let _ = Fiber.continue fiber (create ()) in
    Fiber.cancel fiber (Unwrapped.to_janet (Unwrapped.String "cancelled"));
    Printf.printf "can_resume=%b\n" (Fiber.can_resume fiber));
  [%expect {| can_resume=false |}]
;;

let%expect_test "Fiber.cancel on a New fiber prevents it from running" =
  with_janet_env (fun env ->
    let _ = Janet_ml.dostring_exn ~env "(defn work [] 42)" ~source_path:None in
    let fiber = Fiber.create (lookup_fn env "work") ~capacity:64 ~argv:[] in
    Fiber.cancel fiber (Unwrapped.to_janet (Unwrapped.String "preempted"));
    match Fiber.status fiber with
    | Fiber.Error -> print_endline "Error"
    | s -> Fiber.sexp_of_status s |> Sexp.to_string_hum |> print_endline);
  [%expect {| Error |}]
;;

let%expect_test "driving a generator fiber through all yields" =
  with_janet_env (fun env ->
    let _ =
      Janet_ml.dostring_exn
        ~env
        "(defn counter [] (yield 10) (yield 20) (yield 30))"
        ~source_path:None
    in
    let fiber = Fiber.create (lookup_fn env "counter") ~capacity:64 ~argv:[] in
    let results = ref [] in
    let rec drive () =
      if Fiber.can_resume fiber
      then (
        let sig_, v = Fiber.continue fiber (create ()) in
        match sig_ with
        | Janet_c.C.Types.Signal_yield ->
          results
          := (v |> Unwrapped.of_janet |> Unwrapped.sexp_of_t |> Sexp.to_string_hum)
             :: !results;
          drive ()
        | _ -> ())
    in
    drive ();
    List.rev !results |> List.iter ~f:print_endline);
  [%expect
    {|
    (Number 10)
    (Number 20)
    (Number 30)
    |}]
;;
