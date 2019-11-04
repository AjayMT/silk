
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
    | Ok exprs ->
       let _ = List.map
                 (fun ir_root ->
                   match ir_root with
                   | Codegen.StaticDecl (_, name, _) -> print_string name
                   | Codegen.FuncDecl (_, name, _, _) -> print_string name)
                 exprs in
       print_string "\n"
  with
  | Lexer.SyntaxError s -> prerr_string ("Syntax Error "  ^ s ^ "\n")
