
let () =
  let module F = Janet_c.C.Functions in
  let module T = Janet_c.C.Types in
  let rc = F.janet_init () in
  if rc <> 0 then failwith "janet_init failed";
  Printf.printf
    "Janet version: %d.%d.%d\n"
    T.janet_version_major
    T.janet_version_minor
    T.janet_version_patch;
  let env = F.janet_core_env None in
  let out = Ctypes.allocate_n T.janet ~count:1 in
  let _ = F.janet_dostring env "(+ 1 2)" (Some "main") (Some out) in
  let result = Ctypes.(!@out) in
  (match F.janet_type result with
   | T.Number -> Printf.printf "Result: %f\n" (F.janet_unwrap_number result)
   | T.Nil -> Printf.printf "Result: nil\n"
   | _ -> Printf.printf "Result: <other type>\n");
  F.janet_deinit ()
;;
