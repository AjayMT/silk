
open Symtab

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.file Lexer.token lexbuf in
    let ir_tree = Result.bind
                    (Symtab.construct_symtab ast)
                    (Codegen.construct_ir_tree ast)
    in
    match ir_tree with
    | Error e -> prerr_string e
    | Ok irt ->
       print_string (Codegen.serialize_irt irt)
  with
  | Lexer.SyntaxError s -> prerr_string ("Syntax Error "  ^ s ^ "\n")
