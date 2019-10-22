
module SymTab = Map.Make(String)

type ('a, 'b) either = Left of 'a  | Right of 'b

type valness = Val | Var

type silktype = Int | Void | Function of (silktype list) * silktype
                | NewType of string * silktype

type symbol = Value of valness * silktype * (
                         Parsetree.expr,
                         Parsetree.statement * symbol SymTab.t
                       ) either
            | Type of silktype

let rec fold_left_bind f acc l = match l with
  | [] -> Ok acc
  | (x :: xs) -> Result.bind (f acc x) (fun a -> fold_left_bind f a xs)

let silktype_of_literal_type l = match l with
  | Parsetree.LInt _ -> Int

let rec compare_types a b = match (a, b) with
  | (Int, Int) -> true
  | (Void, Void) -> true
  | (Function (aargtypes, arettype), Function (bargtypes, brettype)) ->
     let f b t1 t2 = if b then compare_types t1 t2 else b in
     (List.fold_left2 f true aargtypes bargtypes) && (compare_types arettype brettype)
  | (NewType (aname, atype), NewType (bname, btype)) ->
     (aname == bname) && (compare_types atype btype)
  | (_, _) -> false

let rec eval_expr_type symtab expr = match expr with
  | Parsetree.Identifier name ->
     begin
       match SymTab.find_opt name symtab with
       | Some (Type _) -> Error ("Error: Expected value, found type: " ^ name)
       | Some (Value (_, t, _)) -> Ok t
       | None -> Error ("Error: Identifier " ^ name ^ " not defined")
     end
  | Parsetree.Literal l -> Ok (silktype_of_literal_type l)
  | Parsetree.Assignment (n, e) ->
     begin
       match (eval_expr_type symtab (Identifier n), eval_expr_type symtab e) with
       | (Error e, _) -> Error e
       | (_, Error e) -> Error e
       | (Ok a, Ok b) ->
          if compare_types a b then Ok a else
            Error ("Error: Mismatched types in assignment of " ^ n)
     end
  | Parsetree.FunctionCall (f, args) ->
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
           List.fold_left2 match_types (Ok Int) argtypes exprs
         else Error ("Error: Incorrect number of arguments")
       in
       let check_function_type stype = match stype with
         | Function (argtypes, t) ->
            Result.bind (match_arg_types argtypes args) (fun _ -> Ok t)
         | _ -> Error ("Error: Expression is not a function")
       in
       Result.bind (eval_expr_type symtab f) check_function_type
     end
  | Parsetree.BinOp (a, op, b) ->
     begin
       match (eval_expr_type symtab a, op, eval_expr_type symtab b) with
       | (Error e, _, _) -> Error e
       | (_, _, Error e) -> Error e
       | (Ok Int, Plus, Ok Int) -> Ok Int
       | _ -> Error ("Error: Incorrect types for binary operation")
     end
  | Parsetree.Index (array, idx) -> Ok Int

let construct_block_symtab symtab_stack stmt =
  let trav_stmt symtab_stack stmt = SymTab.empty in
  Ok SymTab.empty

let construct_symtab ast =
  let silktype_of_asttype symtab t = match t with
    | Parsetree.Int -> Ok Int
    | Parsetree.Void -> Ok Void
    | Parsetree.NewType (name) ->
       match SymTab.find_opt name symtab with
       | Some (Type t) -> Ok (NewType (name, t))
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
                   Error ("Error: mismatched types in declaration of " ^ ident)))
    in

    match vd with
    | Parsetree.ValI (ident, expr) -> check_inferred_type Val symtab ident expr
    | Parsetree.Val (ident, asttype, expr) ->
       check_declared_type Val symtab ident asttype expr
    | Parsetree.VarI (ident, expr) -> check_inferred_type Var symtab ident expr
    | Parsetree.Var (ident, asttype, expr) ->
       check_declared_type Var symtab ident asttype expr
  in

  let trav_funcdecl symtab fd =
    let Parsetree.Func (ident, arglist, ret_asttype, body) = fd in
    match SymTab.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
    | None ->
       let argtypes_unresolved =
         List.map
           (fun (_, asttype) -> silktype_of_asttype symtab asttype)
           arglist
       in
       let argtypes_resolver acc rt =
         Result.map (fun t -> t :: acc) rt
       in
       let argtypes =
         Result.map List.rev
           (fold_left_bind argtypes_resolver [] argtypes_unresolved)
       in
       let rettype = silktype_of_asttype symtab ret_asttype in
       match (argtypes, rettype) with
       | (Ok ats, Ok rt) ->
          Result.bind (construct_block_symtab [symtab] body)
            (fun st ->
              Ok (SymTab.add
                    ident
                    (Value (Val, Function (ats, rt),
                            Right (body, st)))
                    symtab))
       | (Error e, _) -> Error e
       | (_, Error e) -> Error e
  in

  let trav_ast symtab decl = match (symtab, decl) with
    | (symtab, Parsetree.TypeDef (Type (ident, basetype))) ->
       begin
         match SymTab.find_opt ident symtab with
         | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
         | None -> Result.map (fun t -> SymTab.add ident (Type t) symtab)
                     (silktype_of_asttype symtab basetype)
       end
    | (symtab, ValDecl vd) -> trav_valdecl symtab vd
    | (symtab, FuncDecl fd) -> trav_funcdecl symtab fd
  in
  fold_left_bind trav_ast SymTab.empty ast



let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let result = Parser.file Lexer.token lexbuf in
    match construct_symtab result with
    | Ok symtab -> print_string "Ok!\n"
    | Error e -> prerr_string e
  with
  | Lexer.SyntaxError s -> prerr_string ("Syntax Error "  ^ s ^ "\n")
