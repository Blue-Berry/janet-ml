let () =
  let rc = Janet_ml.C.Functions.janet_init () in
  if rc <> 0 then failwith "janet_init failed";
  Printf.printf "Janet version: %d.%d.%d\n"
    Janet_ml.C.Types.janet_version_major
    Janet_ml.C.Types.janet_version_minor
    Janet_ml.C.Types.janet_version_patch;
  Janet_ml.C.Functions.janet_deinit ()


let () =
  let module F = Janet_ml.C.Functions in
  let _ = Janet_ml.C.Functions.janet_init () in
  let env = Janet_ml.C.Functions.janet_core_env None in
  let out = Janet_ml.C.Types.janet in
  F.janet_dostring env "(print `hello, world!`)" ( Some "main") (Some out) |> ignore;
  F.janet_deinit ()
  
