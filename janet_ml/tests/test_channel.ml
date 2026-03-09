(** Tests for the Channel module. *)

open! Core
open Janet_ml
open Janet_ml.Janet

let lookup_fn env name =
  match Env.Binding.lookup ~env name |> Env.Binding.to_janet |> Unwrapped.of_janet with
  | Unwrapped.Function f -> f
  | _ -> failwith (Printf.sprintf "'%s' is not a function" name)
;;

let%expect_test "Channel.make creates a channel" =
  with_janet (fun () ->
    let ch = Channel.make () in
    Printf.printf "not_null=%b\n" (not (Ctypes.is_null ch)));
  [%expect {| not_null=true |}]
;;

let%expect_test "Channel.make_threaded creates a channel" =
  with_janet (fun () ->
    let ch = Channel.make_threaded () in
    Printf.printf "not_null=%b\n" (not (Ctypes.is_null ch)));
  [%expect {| not_null=true |}]
;;

let%expect_test "Channel.make with limit creates a bounded channel" =
  with_janet (fun () ->
    let ch = Channel.make ~limit:5 () in
    Printf.printf "not_null=%b\n" (not (Ctypes.is_null ch)));
  [%expect {| not_null=true |}]
;;

let%expect_test "Channel.to_janet wraps as abstract" =
  with_janet (fun () ->
    let ch = Channel.make () in
    let j = Channel.to_janet ch in
    Printf.printf
      "type=%s\n"
      (match typeof j with
       | Abstract -> "abstract"
       | _ -> "other"));
  [%expect {| type=abstract |}]
;;

let%expect_test "give and take via event loop" =
  with_janet_env (fun env ->
    let ch = Channel.make () in
    (* Define a Janet function that gives a value to the channel *)
    let _ =
      dostring_exn
        ~env
        {|(defn producer [ch] (ev/give ch 42)
           (ev/give ch 99))|}
    in
    (* Define a Janet function that takes values from the channel *)
    let _ =
      dostring_exn
        ~env
        {|(defn consumer [ch]
           (def a (ev/take ch))
           (def b (ev/take ch))
           (+ a b))|}
    in
    let ch_janet = Channel.to_janet ch in
    let prod_fiber =
      Fiber.create (lookup_fn env "producer") ~capacity:64 ~argv:[ ch_janet ]
    in
    let cons_fiber =
      Fiber.create (lookup_fn env "consumer") ~capacity:64 ~argv:[ ch_janet ]
    in
    Ev.schedule prod_fiber (create ());
    Ev.schedule cons_fiber (create ());
    Ev.run ();
    let result = Fiber.last_value cons_fiber in
    Printf.printf "sum=%s\n" (to_string result));
  [%expect {| sum=141 |}]
;;

let%expect_test "OCaml channel passed to Janet preserves FIFO order" =
  with_janet_env (fun env ->
    let ch = Channel.make () in
    let ch_j = Channel.to_janet ch in
    let _ =
      dostring_exn
        ~env
        {|(defn send-three [ch]
           (ev/give ch :first)
           (ev/give ch :second)
           (ev/give ch :third))|}
    in
    let _ =
      dostring_exn
        ~env
        {|(defn recv-three [ch]
           (def a (ev/take ch))
           (def b (ev/take ch))
           (def c (ev/take ch))
           (string a "," b "," c))|}
    in
    let sender = Fiber.create (lookup_fn env "send-three") ~capacity:64 ~argv:[ ch_j ] in
    let recver = Fiber.create (lookup_fn env "recv-three") ~capacity:64 ~argv:[ ch_j ] in
    Ev.schedule sender (create ());
    Ev.schedule recver (create ());
    Ev.run ();
    Printf.printf "%s\n" (to_string_value (Fiber.last_value recver)));
  [%expect {| first,second,third |}]
;;

let%expect_test "OCaml channel: OCaml gives, Janet fiber takes" =
  with_janet_env (fun env ->
    let ch = Channel.make () in
    let ch_j = Channel.to_janet ch in
    (* OCaml cfun gives values to the channel *)
    let h =
      Cfun.register ~env "ocaml-produce" (fun _args ->
        let _ = Channel.give ch (of_string "hello") in
        let _ = Channel.give ch (of_string "world") in
        nil)
    in
    let _ = dostring_exn ~env "(defn do-produce [] (ocaml-produce))" in
    let _ =
      dostring_exn
        ~env
        {|(defn janet-consume [ch]
           (def a (ev/take ch))
           (def b (ev/take ch))
           (string a " " b))|}
    in
    let producer = Fiber.create (lookup_fn env "do-produce") ~capacity:64 ~argv:[] in
    let consumer =
      Fiber.create (lookup_fn env "janet-consume") ~capacity:64 ~argv:[ ch_j ]
    in
    Ev.schedule producer (create ());
    Ev.schedule consumer (create ());
    Ev.run ();
    Printf.printf "%s\n" (to_string_value (Fiber.last_value consumer));
    Cfun.free h);
  [%expect {| hello world |}]
;;

let%expect_test "OCaml channel: Janet fiber gives, OCaml takes" =
  with_janet_env (fun env ->
    let ch = Channel.make () in
    let ch_j = Channel.to_janet ch in
    let result = ref "" in
    let h =
      Cfun.register ~env "ocaml-consume" (fun _args ->
        (match Channel.take ch with
         | Some v -> result := to_string_value v
         | None -> result := "NONE");
        nil)
    in
    let _ = dostring_exn ~env "(defn do-consume [] (ocaml-consume))" in
    let _ = dostring_exn ~env {|(defn janet-produce [ch] (ev/give ch "from-janet"))|} in
    let producer =
      Fiber.create (lookup_fn env "janet-produce") ~capacity:64 ~argv:[ ch_j ]
    in
    let consumer = Fiber.create (lookup_fn env "do-consume") ~capacity:64 ~argv:[] in
    Ev.schedule producer (create ());
    Ev.schedule consumer (create ());
    Ev.run ();
    Printf.printf "result=%s\n" !result;
    Cfun.free h);
  [%expect {| result=from-janet |}]
;;

let%expect_test "OCaml channel carries different Janet types" =
  with_janet_env (fun env ->
    let ch = Channel.make () in
    let ch_j = Channel.to_janet ch in
    let _ =
      dostring_exn
        ~env
        {|(defn send-types [ch]
           (ev/give ch 3.14)
           (ev/give ch true)
           (ev/give ch nil)
           (ev/give ch :kw))|}
    in
    let _ =
      dostring_exn
        ~env
        {|(defn recv-types [ch]
           (def a (ev/take ch))
           (def b (ev/take ch))
           (def c (ev/take ch))
           (def d (ev/take ch))
           (string/format "%n,%n,%n,%n" a b c d))|}
    in
    let sender = Fiber.create (lookup_fn env "send-types") ~capacity:64 ~argv:[ ch_j ] in
    let recver = Fiber.create (lookup_fn env "recv-types") ~capacity:64 ~argv:[ ch_j ] in
    Ev.schedule sender (create ());
    Ev.schedule recver (create ());
    Ev.run ();
    Printf.printf "%s\n" (to_string_value (Fiber.last_value recver)));
  [%expect {| 3.14,true,nil,:kw |}]
;;

let%expect_test "two OCaml channels used independently" =
  with_janet_env (fun env ->
    let ch_a = Channel.make () in
    let ch_b = Channel.make () in
    let ch_a_j = Channel.to_janet ch_a in
    let ch_b_j = Channel.to_janet ch_b in
    let _ =
      dostring_exn
        ~env
        {|(defn route [a b]
           (ev/give a 10)
           (ev/give b 20))|}
    in
    let _ =
      dostring_exn
        ~env
        {|(defn collect [a b]
           (def x (ev/take a))
           (def y (ev/take b))
           (+ x y))|}
    in
    let sender =
      Fiber.create (lookup_fn env "route") ~capacity:64 ~argv:[ ch_a_j; ch_b_j ]
    in
    let recver =
      Fiber.create (lookup_fn env "collect") ~capacity:64 ~argv:[ ch_a_j; ch_b_j ]
    in
    Ev.schedule sender (create ());
    Ev.schedule recver (create ());
    Ev.run ();
    Printf.printf "sum=%s\n" (to_string (Fiber.last_value recver)));
  [%expect {| sum=30 |}]
;;

let%expect_test "OCaml threaded channel passed to Janet" =
  with_janet_env (fun env ->
    let ch = Channel.make_threaded () in
    let ch_j = Channel.to_janet ch in
    let _ = dostring_exn ~env {|(defn thr-send [ch] (ev/give ch :threaded))|} in
    let _ = dostring_exn ~env {|(defn thr-recv [ch] (ev/take ch))|} in
    let sender = Fiber.create (lookup_fn env "thr-send") ~capacity:64 ~argv:[ ch_j ] in
    let recver = Fiber.create (lookup_fn env "thr-recv") ~capacity:64 ~argv:[ ch_j ] in
    Ev.schedule sender (create ());
    Ev.schedule recver (create ());
    Ev.run ();
    Printf.printf "%s\n" (to_string (Fiber.last_value recver)));
  [%expect {| :threaded |}]
;;

let%expect_test "Channel give/take roundtrip with OCaml API" =
  with_janet_env (fun env ->
    let ch = Channel.make () in
    (* Use a fiber that gives a value to the channel from OCaml *)
    let give_fn =
      Cfun.register ~env "test-give" (fun _args ->
        let _ = Channel.give ch (of_int 77) in
        nil)
    in
    let take_fn =
      Cfun.register ~env "test-take" (fun _args ->
        match Channel.take ch with
        | Some v -> v
        | None -> nil)
    in
    let _ = dostring_exn ~env "(defn do-give [] (test-give))" in
    let _ = dostring_exn ~env "(defn do-take [] (test-take))" in
    let giver = Fiber.create (lookup_fn env "do-give") ~capacity:64 ~argv:[] in
    let taker = Fiber.create (lookup_fn env "do-take") ~capacity:64 ~argv:[] in
    Ev.schedule giver (create ());
    Ev.schedule taker (create ());
    Ev.run ();
    let result = Fiber.last_value taker in
    Printf.printf "took=%s\n" (to_string result);
    Cfun.free give_fn;
    Cfun.free take_fn);
  [%expect {| took=77 |}]
;;
