
open Parsetree

type silktype = SInt | SVoid | SNewType of string * silktype

type symtype = Value | Type of silktype

let construct_symtab ast =
  let extract_v_name vd = match vd with
    | ValI (name, _) -> name
    | Val (name, _, _) -> name
    | VarI (name, _) -> name
    | Var (name, _, _) -> name
  in
  let trav_symbol table astnode = match astnode with
    | FuncDecl (Func (name, _, _, _)) -> name :: table
    | ValDecl v -> (extract_v_name v) :: table
    | TypeDef (Type (name, _)) -> name :: table
  in
  List.fold_left trav_symbol [] ast

let print_funcdecl f =
  let print_args args = List.map (fun (name, t) -> print_string (name ^ " ")) args in
  let _ = match f with
    | Func (name, args, rt, body) -> print_string (name ^ ": "); print_args args
  in ()

let print_ast decl = match decl with
  | TypeDef _  -> print_string "typedef\n"
  | ValDecl _  -> print_string "valdecl\n"
  | FuncDecl f -> print_funcdecl f; print_string "\n"

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let result = Parser.file Lexer.token lexbuf in
    let _ = List.map (fun x -> print_string (x ^ " ")) (construct_symtab result) in
    ()
  with
  | Lexer.SyntaxError s -> prerr_string ("Syntax Error "  ^ s ^ "\n")
