
let ( let* ) x f = Result.bind x f

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let ast = Template.process_file @@ Parser.file Lexer.token lexbuf in
    let ir_tree = Result.bind
                    (Symtab.construct_symtab ast)
                    (Codegen.construct_ir_tree ast)
    in
    let code = Result.bind ir_tree Codegen.serialize_irt in
    match code with
    | Ok s -> print_string s
    | Error e -> prerr_string e
  with
  | Lexer.SyntaxError s -> prerr_string ("Syntax Error "  ^ s ^ "\n")
