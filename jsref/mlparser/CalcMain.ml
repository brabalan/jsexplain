open CalcSyntax
open CalcParser
open CalcInterpreter

let print_location loc =
  print_char '(' ;
  print_int loc.start.line ;
  print_string ", " ;
  print_int loc.start.column ;
  print_string "): "

let () =
  let code = Sys.argv.(1) in
  let ast = parse_string "test" code in
  match eval_expr ast with
  (* | Left { loc = loc ; value = err } -> prerr_endline err *)
  | Left err -> prerr_endline err
  | Right lit ->
    print_location (get_literal_location lit) ;
    print_float (float_of_literal lit) ;
    print_newline ()
