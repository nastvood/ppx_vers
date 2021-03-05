let tests = [
  ("abstract", []);
  ("variant", []);
  ("record_vers", []);
  ("record_novers", []);
  ("pvariant", []);
  ("include_type", []);
  ("with_bin_io", ["bin_prot"]);
]

let run cmd = 
  let cin = Unix.open_process_in cmd in
  let rec loop cin res =
    match Stdlib.input_line cin with
    | s ->      
      loop cin (s :: res)
    | exception End_of_file -> 
      Stdlib.close_in cin;
      String.concat "\n" (List.rev res)
  in  
  loop cin []

let get_diff s1 s2 =
  let (tmp_s1, cout1) = Filename.open_temp_file  "s1" ".ml" in
  let (tmp_s2, cout2) = Filename.open_temp_file  "s2" ".ml" in
  Stdlib.output_string cout1 s1;
  Stdlib.output_string cout2 s2;
  Stdlib.close_out cout1;
  Stdlib.close_out cout2;
  run (Printf.sprintf "diff -u %s %s" tmp_s1 tmp_s2)

let ocamlfind_query libs =
  if libs = []
  then ""
  else
    let str_libs =
      List.map (fun l ->
        let res = run (Printf.sprintf "ocamlfind query %s" l) in
        "-I " ^ res
      ) libs
    in
    Printf.sprintf "%s -c -impl" (String.concat " " str_libs)

let () = 
  let rex = Pcre.regexp "\\[@@@ocaml.ppx.context[\\d\\D]*}]\n" in 
  List.iter (fun (s, libs) ->
    let _ = run (Printf.sprintf "dune build test/%s.cma" s) in 
    let dsource_cmd = Printf.sprintf "ocamlc -dsource %s _build/default/test/%s.pp.ml" (ocamlfind_query libs) s in
    let res = run (dsource_cmd ^ " 2>&1") in
    let str = Pcre.replace ~rex res in
    let str_exp = run (Printf.sprintf "cat test/expect/%s.ml" s) in
    if str <> str_exp
    then 
      let diff = get_diff str str_exp in
      Printf.printf "test \x1b[1;33m%s: \x1b[1;31mfail\n\x1b[1;34mrun: %s\x1b[0m\ndiff:\n%s\n" s dsource_cmd diff;   
      exit 1
    else  
      Printf.eprintf "test \x1b[1;33m%s: \x1b[0;32mpassed\x1b[0m\n" s 
  ) tests
