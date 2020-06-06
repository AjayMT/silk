
let ( let* ) x f = Result.bind x f

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let raw_ast = Parser.file Lexer.token lexbuf in
    let ast = Template.process_file raw_ast in
    match ast with
    | Error e -> prerr_string e
    | Ok ast ->
       let ir_tree = Result.bind
                       (Symtab.construct_symtab ast)
                       (Codegen.construct_ir_tree ast)
       in
       let code = Result.bind ir_tree Codegen.serialize_irt in
       match code with
       | Ok s -> print_string s
       | Error e -> prerr_string (e ^ "\n")
  with
  | Lexer.SyntaxError s -> prerr_string ("Syntax Error "  ^ s ^ "\n")
