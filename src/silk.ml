
open Parsetree

module SymTab = Map.Make(String)

type ('a, 'b) either = Left of 'a  | Right of 'b

type valness = VVal | VVar

type silktype = SInt | SVoid | SFunction of (silktype list) * silktype
                | SNewType of string * silktype

type symbol = Value of valness * silktype * (Parsetree.expr, symbol SymTab.t) either
            | Type of silktype

let eval_literal_type l = match l with
  | LInt _ -> SInt

let rec compare_types a b = match (a, b) with
  | (SInt, SInt) -> true
  | (SVoid, SVoid) -> true
  | (SFunction (aargtypes, arettype), SFunction (bargtypes, brettype)) ->
     let f b t1 t2 = if b then compare_types t1 t2 else b in
     (List.fold_left2 f true aargtypes bargtypes) && (compare_types arettype brettype)
  | (SNewType (aname, atype), SNewType (bname, btype)) ->
     (aname == bname) && (compare_types atype btype)
  | (_, _) -> false

let rec eval_expr_type symtab expr = match expr with
  | Identifier name ->
     begin
       match SymTab.find_opt name symtab with
       | Some (Type _) -> Error ("Error: Expected value, found type: " ^ name)
       | Some (Value (_, t, _)) -> Ok t
       | None -> Error ("Error: Identifier " ^ name ^ " not defined")
     end
  | Literal l -> Ok (eval_literal_type l)
  | Assignment (n, e) ->
     begin
       match (eval_expr_type symtab (Identifier n), eval_expr_type symtab e) with
       | (Error e, _) -> Error e
       | (_, Error e) -> Error e
       | (Ok a, Ok b) ->
          if compare_types a b then Ok a else
            Error ("Error: Mismatched types in assignment of " ^ n)
     end
  | FunctionCall (f, args) ->
     begin
       let match_arg_types argtypes exprs =
         let match_types acc t exp = match (acc, t, exp) with
           | (Error e, _, _) -> Error e
           | (_, t, exp)  ->
              let check_arg_type et =
                if compare_types et t then Ok t
                else Error ("Error: Mismatched types in function call")
              in
              Result.bind (eval_expr_type symtab exp) check_arg_type
         in
         if List.length argtypes == List.length args then
           List.fold_left2 match_types (Ok SInt) argtypes exprs
         else Error ("Error: Incorrect number of arguments")
       in
       let check_function_type stype = match stype with
         | SFunction (argtypes, t) ->
            Result.bind (match_arg_types argtypes args) (fun _ -> Ok t)
         | _ -> Error ("Error: Expression is not a function")
       in
       Result.bind (eval_expr_type symtab f) check_function_type
     end
  | BinOp (a, op, b) ->
     begin
       match (eval_expr_type symtab a, op, eval_expr_type symtab b) with
       | (Error e, _, _) -> Error e
       | (_, _, Error e) -> Error e
       | (Ok SInt, Plus, Ok SInt) -> Ok SInt
       | _ -> Error ("Error: Incorrect types for binary operation")
     end
  | Index (array, idx) -> Ok SInt

let construct_symtab ast =
  let silktype_of_asttype symtab t = match t with
    | Int -> Ok SInt
    | Void -> Ok SVoid
    | NewType (name) -> match SymTab.find_opt name symtab with
                        | Some (Type t) -> Ok (SNewType (name, t))
                        | Some (Value _) ->
                           Error ("Error: " ^ name ^ " is not a type")
                        | None -> Error ("Error: type " ^ name ^ " undefined")
  in

  let trav_valdecl symtab vd =
    let check_inferred_type mut symtab ident expr =
      match SymTab.find_opt ident symtab with
      | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
      | None ->
         Result.map (
             fun stype -> SymTab.add ident (Value (mut, stype, Left expr)) symtab
           ) (eval_expr_type symtab expr)
    in

    let check_declared_type mut symtab ident asttype expr =
      match SymTab.find_opt ident symtab with
      | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
      | None ->
         let lefttype = silktype_of_asttype symtab asttype in
         let righttype = eval_expr_type symtab expr in
         Result.bind righttype (
             fun rstype ->
             Result.bind lefttype (fun lstype ->
                 if compare_types lstype rstype then
                   Ok (SymTab.add ident (Value (mut, lstype, Left expr)) symtab)
                 else
                   Error ("Error: mismatched types in declaration of " ^ ident)
               )
           )
    in

    match vd with
    | ValI (ident, expr) -> check_inferred_type VVal symtab ident expr
    | Val (ident, asttype, expr) -> check_declared_type VVal symtab ident asttype expr
    | VarI (ident, expr) -> check_inferred_type VVar symtab ident expr
    | Var (ident, asttype, expr) -> check_declared_type VVar symtab ident asttype expr
  in

  let trav_funcdecl symtab fd =
    let Func (ident, arglist, ret_asttype, body) = fd in
    match SymTab.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
    | None ->
       let argtypes_unresolved =
         List.map
           (fun (_, asttype) -> silktype_of_asttype symtab asttype)
           arglist
       in
       let argtypes_resolver acc rt =
         Result.bind
           acc
           (fun l -> Result.map (fun t -> t :: l) rt)
       in
       let argtypes =
         Result.map List.rev
           (List.fold_left argtypes_resolver (Ok []) argtypes_unresolved)
       in
       let rettype = silktype_of_asttype symtab ret_asttype in
       match (argtypes, rettype) with
       | (Ok ats, Ok rt) ->
          Ok (SymTab.add
                ident
                (Value (VVal, SFunction (ats, rt), Right SymTab.empty))
                symtab)
       | (Error e, _) -> Error e
       | (_, Error e) -> Error e
  in

  let trav_ast rsymtab decl = match (rsymtab, decl) with
    | (Error e, _) -> Error e
    | (Ok symtab, TypeDef (Type (ident, rt))) ->
       begin
         match SymTab.find_opt ident symtab with
         | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
         | None -> match silktype_of_asttype symtab rt with
                   | Ok rst -> Ok (SymTab.add ident (Type rst) symtab)
                   | Error e -> Error e
       end
    | (Ok symtab, ValDecl vd) -> trav_valdecl symtab vd
    | (Ok symtab, FuncDecl fd) -> trav_funcdecl symtab fd
  in
  List.fold_left trav_ast (Ok SymTab.empty) ast


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
    match construct_symtab result with
    | Ok symtab -> print_string "Ok!\n"
    | Error e -> prerr_string e
  with
  | Lexer.SyntaxError s -> prerr_string ("Syntax Error "  ^ s ^ "\n")
