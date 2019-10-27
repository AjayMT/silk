
open Symtab

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.file Lexer.token lexbuf in
    let sc = Result.map Codegen.serialize_code (Codegen.codegen_ast ast)
    in
    match sc with
    | Error e -> prerr_string e
    | Ok code ->
       let _ = List.map print_string code
       in ()
  with
  | Lexer.SyntaxError s -> prerr_string ("Syntax Error "  ^ s ^ "\n")
