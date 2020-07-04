
(**
 * CLI and compiler driver.
 *)

module Parser = Nice_parser.Make(struct
                    type result = Parsetree.top_decl list
                    type token = Menhir_parser.token
                    exception ParseError = Menhir_parser.Error
                    exception LexError = Ocamllex_lexer.LexError
                    let parse = Menhir_parser.file
                    let next_token = Ocamllex_lexer.token
                  end)

let ( let* ) x f = Result.bind x f

let compile file =
  Parser.pp_exceptions ();
  let raw_ast = match file with
    | Some file -> Parser.parse_file file
    | None -> Parser.parse_chan stdin
  in
  let* ast = Template.process_file raw_ast in
  let* symtab = Symtab.construct_symtab ast in
  let* ir_tree = Codegen.construct_ir_tree ast symtab in
  Codegen.serialize_irt ir_tree

let _ =
  let output = ref None in
  let input_stdin = ref false in
  let help = ref false in
  let version = ref false in
  let files = ref [] in

  let process_file f =
    let outf = match !output with
      | Some file ->
         fun s ->
         let ch = open_out file in
         Printf.fprintf ch "%s" s
      | None -> print_string
    in
    match compile f with
    | Ok out -> outf out
    | Error err ->
       let no_color = Sys.getenv_opt "NO_COLOR" in
       let prefix =
         if Option.is_some no_color then "[compiler]"
         else "\027[1;36m[compiler]\027[0m"
       in
       let suffix = match f with
         | Some f -> "in file '" ^ f ^ "'"
         | None -> "in input"
       in
       let err =
         if Option.is_some no_color then err
         else
           let l = String.length "Error" in
           "\027[1;31mError\027[0m" ^ (String.sub err l ((String.length err) - l))
       in
       let err = String.split_on_char '\n' err in

       Format.set_formatter_out_channel stderr;
       Format.open_box 0;
       Format.print_string prefix; Format.force_newline ();
       Format.open_box 2;
       List.iter (fun s -> Format.print_string s; Format.force_newline ()) err;
       Format.close_box ();
       Format.print_string suffix; Format.print_newline ();
       Format.close_box ();
  in

  let usage_msg =
    "The Silk compiler.\n\n"
    ^ "This program compiles one or more silk files (or input read\n"
    ^ "from stdin) and writes LLVM to stdout or the specified output\n"
    ^ "file.\n\n"
    ^ "Visit http://ajaymt.github.io/silk for documentation.\n\n"
    ^ "Usage: silk [options] [file...]\n\n"
    ^ "Options:"
  in
  let opt_list = [
      ("-o", Arg.String (fun o -> output := Some o),
       "<file>\tOutput file [default: stdout]");
      ("-", Arg.Set input_stdin, "\t\tRead input from stdin [default: false]");
      ("--version", Arg.Set version, "\tPrint version number and exit");
      ("--help", Arg.Set help, "\tPrint this help message and exit");
      ("-help", Arg.Set help, "")
    ] in

  let () = Arg.parse opt_list (fun f -> files := f :: !files) usage_msg in

  match (!version, !help, List.length !files, !input_stdin) with
  | (true, _, _, _) -> print_string "silk version 1.0.0-alpha\n"
  | (_, true, _, _) -> Arg.usage opt_list usage_msg
  | (_, _, 0, false) -> Arg.usage opt_list usage_msg; exit 1
  | _ ->
     let () = List.iter (fun f -> process_file @@ Some f) !files in
     if !input_stdin then process_file None else ()
