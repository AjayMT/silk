
open Symtab

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let result = Parser.file Lexer.token lexbuf in
    match construct_symtab result with
    | Ok symtab -> print_string "Ok!\n"
    | Error e -> prerr_string e
  with
  | Lexer.SyntaxError s -> prerr_string ("Syntax Error "  ^ s ^ "\n")
