
module SymTab = Map.Make(String)

type ('a, 'b) either = Left of 'a  | Right of 'b

type valness = Val | Var

type silktype = Int | Bool | Void | Function of (silktype list) * silktype
                | NewType of string * silktype

type symbol = Value of valness * silktype *
                         (Parsetree.expr,
                          Parsetree.statement * symbol SymTab.t) either
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

let rec find_symtab_stack name ststack  = match ststack with
  | [] -> None
  | (z :: zs) ->
     match SymTab.find_opt name z with
     | Some v -> Some v
     | None -> find_symtab_stack name zs

let rec eval_expr_type symtab_stack expr = match expr with
  | Parsetree.Identifier name ->
     begin
       match find_symtab_stack name symtab_stack with
       | Some (Type _) -> Error ("Error: Expected value, found type: " ^ name)
       | Some (Value (_, t, _)) -> Ok t
       | None -> Error ("Error: Identifier " ^ name ^ " not defined")
     end
  | Parsetree.Literal l -> Ok (silktype_of_literal_type l)
  | Parsetree.Assignment (n, e) ->
     begin
       match (eval_expr_type symtab_stack (Identifier n),
              eval_expr_type symtab_stack e) with
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
              Result.bind (eval_expr_type symtab_stack exp) check_arg_type
         in
         if List.length argtypes == List.length args then
           List.fold_left2 match_types (Ok Int) argtypes exprs
         else Error ("Error: Incorrect number of arguments")
       in
       let check_function_type stype = match stype with
         | Function (argtypes, t) -> Result.map
                                       (fun _ -> t)
                                       (match_arg_types argtypes args)
         | _ -> Error ("Error: Expression is not a function")
       in
       Result.bind (eval_expr_type symtab_stack f) check_function_type
     end
  | Parsetree.BinOp (a, op, b) ->
     begin
       match (eval_expr_type symtab_stack a, op, eval_expr_type symtab_stack b) with
       | (Error e, _, _) -> Error e
       | (_, _, Error e) -> Error e
       | (Ok Int, Plus, Ok Int) -> Ok Int
       | _ -> Error ("Error: Incorrect types for binary operation")
     end
  | Parsetree.Index (array, idx) -> Ok Int

let silktype_of_asttype symtab t = match t with
  | Parsetree.Int -> Ok Int
  | Parsetree.Void -> Ok Void
  | Parsetree.NewType (name) ->
     match SymTab.find_opt name symtab with
     | Some (Type t) -> Ok (NewType (name, t))
     | Some (Value _) ->
        Error ("Error: " ^ name ^ " is not a type")
     | None -> Error ("Error: type " ^ name ^ " undefined")

let trav_valdecl symtab symtab_stack types_tab vd =
  let check_inferred_type mut ident expr =
    match SymTab.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
    | None ->
       Result.map
         (fun stype -> SymTab.add ident (Value (mut, stype, Left expr)) symtab)
         (eval_expr_type (symtab :: symtab_stack) expr)
  in

  let check_declared_type mut ident asttype expr =
    match SymTab.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
    | None ->
       let lefttype = silktype_of_asttype types_tab asttype in
       let righttype = eval_expr_type (symtab :: symtab_stack) expr in
       Result.bind righttype
         (fun rstype ->
           Result.bind lefttype (fun lstype ->
               if compare_types lstype rstype then
                 Ok (SymTab.add ident (Value (mut, lstype, Left expr)) symtab)
               else
                 Error ("Error: mismatched types in declaration of " ^ ident)))
  in

  match vd with
  | Parsetree.ValI (ident, expr) -> check_inferred_type Val ident expr
  | Parsetree.Val (ident, asttype, expr) ->
     check_declared_type Val ident asttype expr
  | Parsetree.VarI (ident, expr) -> check_inferred_type Var ident expr
  | Parsetree.Var (ident, asttype, expr) ->
     check_declared_type Var ident asttype expr


let rec construct_block_symtab symtab_stack types_tab stmts =
  let addblk block_number symtab blk =
    let new_symtab st = SymTab.add (string_of_int block_number)
                          (Value
                             (Val, Void,
                              Right (Parsetree.Block blk, st)))
                          symtab
    in
    Result.map
      (fun st -> (block_number + 1, new_symtab st))
      (construct_block_symtab (symtab :: symtab_stack) types_tab blk)
  in

  let trav_stmt acc stmt =
    let (block_number, symtab) = acc in
    match stmt with
    | Parsetree.Empty -> Ok (block_number, symtab)
    | Parsetree.Decl vd -> Result.map
                             (fun s -> (block_number, s))
                             (trav_valdecl symtab symtab_stack types_tab vd)
    | Parsetree.Expr exp -> Result.map
                              (fun _ -> (block_number, symtab))
                              (eval_expr_type (symtab :: symtab_stack) exp)
    | Parsetree.Block blk -> addblk block_number symtab blk
    | Parsetree.IfElse (exp, ifstmt, elsestmt) ->
       begin
         match ifstmt with
         | Parsetree.Block ifblk ->
            Result.bind (eval_expr_type (symtab :: symtab_stack) exp)
              begin
                fun expr_t ->
                match expr_t with
                | Bool ->
                   let ifresult = addblk block_number symtab ifblk in
                   begin
                     match elsestmt with
                     | Parsetree.Block elseblk ->
                        Result.bind ifresult (fun (b, s) -> addblk b s elseblk)
                     | Parsetree.Empty -> ifresult
                     | _ -> Error "Error: Not a block."
                   end
                | _ -> Error "Error: Expected boolean expression in 'if' condition"
              end
         | _ -> Error "Error: Not a block."
       end
    | Parsetree.While (exp, whilestmt) ->
       begin
         match whilestmt with
         | Parsetree.Block blk ->
            Result.bind (eval_expr_type (symtab :: symtab_stack) exp)
              begin
                fun expr_t ->
                match expr_t with
                | Bool -> addblk block_number symtab blk
                | _ -> Error "Error: Expected boolean expression in 'while' condition"
              end
         | _ -> Error "Error: Not a block."
       end
    | Parsetree.For (vd, condexp, incexp, forblk) ->
       begin
         match forblk with
         | Parsetree.Block blk ->
            let local_symtab = SymTab.empty in
            let vd_result =
              trav_valdecl local_symtab (symtab :: symtab_stack) types_tab vd
            in
            Result.bind vd_result
              begin
                fun local_symtab ->
                Result.bind
                  (eval_expr_type (local_symtab :: symtab :: symtab_stack) condexp)
                  begin
                    fun condexp_t ->
                    match condexp_t with
                    (* TODO *)
                    | Bool -> Ok (block_number, symtab)
                    | _ ->
                       Error "Error: Expected boolean expression in 'for' condition"
                  end
              end
         | _ -> Error "Error: Not a block."
       end
    | Parsetree.Return (exo) ->
       begin
         match exo with
         | Some _ -> Ok (block_number, symtab)
         | None -> Ok (block_number, symtab)
       end
    | Parsetree.Continue | Parsetree.Break -> Ok (block_number, symtab)
  in
  Result.map (fun (_, s) -> s) (fold_left_bind trav_stmt (0, SymTab.empty) stmts)

let construct_symtab ast =
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
          begin
            match body with
            | Parsetree.Block blk ->
               Result.map
                 (fun st -> SymTab.add ident
                              (Value (Val, Function (ats, rt),
                                      Right (body, st)))
                              symtab)
                 (construct_block_symtab [symtab] symtab blk)
            | _ -> Error "Error: Not a block."
          end
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
    | (symtab, ValDecl vd) -> trav_valdecl symtab [] symtab vd
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
