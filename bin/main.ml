open Janet_ml

let () =
  with_janet_env (fun env ->
    let open Janet in
    (* Print Janet version using the C constants from the type layer *)
    let v = Janet_c.C.Types.janet_version_major in
    let mi = Janet_c.C.Types.janet_version_minor in
    let p = Janet_c.C.Types.janet_version_patch in
    Printf.printf "Janet version: %d.%d.%d\n" v mi p;

    (* Basic arithmetic *)
    let result = dostring_exn ~env {|(+ 1 2)|} ~source_path:(Some "main") in
    Printf.printf "(+ 1 2) = %s\n" (to_string result);

    (* Define a function and call it *)
    let _r = dostring_exn ~env {|(defn greet [name] (string "Hello, " name "!"))|} ~source_path:(Some "main") in
    let greet_fn =
      match Env.Binding.lookup ~env "greet" |> Env.Binding.to_janet |> Unwrapped.of_janet with
      | Unwrapped.Function f -> f
      | _ -> failwith "greet not found"
    in
    let greeting = Function.call_exn greet_fn [ Unwrapped.to_janet (Unwrapped.String "world") ] in
    Printf.printf "greet: %s\n" (to_string greeting);

    (* Register an OCaml function as a Janet cfunction *)
    let _cb = register_cfun ~env "ocaml-add" (fun argc argv ->
      let args = Ctypes.CArray.from_ptr argv (Int32.to_int argc) in
      let a = Ctypes.CArray.get args 0 |> Janet_c.C.Functions.janet_unwrap_number in
      let b = Ctypes.CArray.get args 1 |> Janet_c.C.Functions.janet_unwrap_number in
      Janet_c.C.Functions.janet_wrap_number (a +. b))
    in
    let ocaml_result = dostring_exn ~env {|(ocaml-add 10 32)|} ~source_path:(Some "main") in
    Printf.printf "(ocaml-add 10 32) = %s\n" (to_string ocaml_result);

    (* Table iteration *)
    let tbl = Table.of_pairs
      [ Unwrapped.to_janet (Unwrapped.Keyword "x"), Unwrapped.to_janet (Unwrapped.Number 1.0)
      ; Unwrapped.to_janet (Unwrapped.Keyword "y"), Unwrapped.to_janet (Unwrapped.Number 2.0)
      ]
    in
    Printf.printf "Table pairs:\n";
    Table.iter tbl ~f:(fun k v ->
      Printf.printf "  %s => %s\n" (to_string k) (to_string v)))
