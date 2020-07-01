
module Parser = Nice_parser.Make(struct
                    type result = Parsetree.top_decl list
                    type token = Menhir_parser.token
                    exception ParseError = Menhir_parser.Error
                    exception LexError = Ocamllex_lexer.LexError
                    let parse = Menhir_parser.file
                    let next_token = Ocamllex_lexer.token
                  end)

let ( let* ) x f = Result.bind x f

let _ =
  Parser.pp_exceptions ();
  let raw_ast = Parser.parse_file Sys.argv.(1) in
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
