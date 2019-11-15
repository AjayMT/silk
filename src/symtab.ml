
(*
 * Type checking and symbol table construction.
 *)

module SymtabM = Map.Make(String)

type valness = Val | Var

type silktype = I32 | U32
                | Bool | Void
                | Function of (silktype list) * silktype
                | NewType of string * silktype

type symbol = Value of valness * silktype * symbol SymtabM.t option
            | Type of silktype

let rec fold_left_bind f acc l = match l with
  | [] -> Ok acc
  | (x :: xs) -> Result.bind (f acc x) (fun a -> fold_left_bind f a xs)

let silktype_of_literal_type l = match l with
  | Parsetree.LI32 _ -> I32
  | Parsetree.LU32 _ -> U32
  | Parsetree.LBool _ -> Bool

let rec compare_types a b = match (a, b) with
  | (Function (aargtypes, arettype), Function (bargtypes, brettype)) ->
     let f b t1 t2 = if b then compare_types t1 t2 else b in
     (List.fold_left2 f true aargtypes bargtypes) && (compare_types arettype brettype)
  | (NewType (aname, atype), NewType (bname, btype)) ->
     (aname == bname) && (compare_types atype btype)
  | (a, b) -> a == b

let rec find_symtab_stack name ststack  = match ststack with
  | [] -> None
  | (z :: zs) ->
     match SymtabM.find_opt name z with
     | Some v -> Some v
     | None -> find_symtab_stack name zs

let rec eval_expr_type symtab_stack expr = match expr with
  | Parsetree.Identifier name ->
     begin
       match find_symtab_stack name symtab_stack with
       | Some (Type _) -> Error ("Error: Expected value, found type: " ^ name)
       | Some (Value (_, t, _)) -> Ok t
       | None -> Error ("Error: Identifier " ^ name ^ " undefined")
     end
  | Parsetree.Literal l -> Ok (silktype_of_literal_type l)
  | Parsetree.Assignment (n, e) ->
     Result.bind (eval_expr_type symtab_stack e)
       begin
         fun exprtype ->
         match (find_symtab_stack n symtab_stack) with
         | Some Value (Var, idtype, _) ->
            if compare_types idtype exprtype then Ok exprtype
            else Error ("Error: Mismatched types in assignment of " ^ n)
         | Some Value (Val, _, _) -> Error ("Error: Cannot re-assign val " ^ n)
         | Some Type _ -> Error ("Error: Expected value, found type: " ^ n)
         | None -> Error ("Error: Identifier " ^ n ^ " undefined")
       end
  | Parsetree.FunctionCall (f, args) ->
     begin
       let match_arg_types argtypes exprs =
         let match_types acc t exp =
           Result.bind acc
             begin
               fun _ ->
               let check_arg_type et =
                 if compare_types et t then Ok t
                 else Error ("Error: Mismatched types in function call")
               in
               Result.bind (eval_expr_type symtab_stack exp) check_arg_type
             end
         in
         if List.length argtypes == List.length args then
           List.fold_left2 match_types (Ok Bool) argtypes exprs
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
       | (Ok I32, Plus, Ok I32) -> Ok I32
       | (Ok I32, Minus, Ok I32) -> Ok I32
       | (Ok I32, Times, Ok I32) -> Ok I32
       | (Ok I32, Divide, Ok I32) -> Ok I32
       | (Ok I32, Modulus, Ok I32) -> Ok I32
       | (Ok I32, Equal, Ok I32) -> Ok Bool
       | (Ok I32, LessThan, Ok I32) -> Ok Bool
       | (Ok I32, GreaterThan, Ok I32) -> Ok Bool

       | (Ok U32, Plus, Ok U32) -> Ok U32
       | (Ok U32, Minus, Ok U32) -> Ok U32
       | (Ok U32, Times, Ok U32) -> Ok U32
       | (Ok U32, Divide, Ok U32) -> Ok U32
       | (Ok U32, Modulus, Ok U32) -> Ok U32
       | (Ok U32, Equal, Ok U32) -> Ok Bool
       | (Ok U32, LessThan, Ok U32) -> Ok Bool
       | (Ok U32, GreaterThan, Ok U32) -> Ok Bool

       | (Ok Bool, And, Ok Bool) -> Ok Bool
       | (Ok Bool, Or, Ok Bool) -> Ok Bool
       | (Ok Bool, Equal, Ok Bool) -> Ok Bool

       | (Ok I32, RShift, Ok I32) -> Ok I32
       | (Ok I32, LShift, Ok I32) -> Ok I32
       | (Ok I32, BitAnd, Ok I32) -> Ok I32
       | (Ok I32, BitOr, Ok I32) -> Ok I32
       | (Ok I32, BitXor, Ok I32) -> Ok I32

       | (Ok U32, RShift, Ok U32) -> Ok U32
       | (Ok U32, LShift, Ok U32) -> Ok U32
       | (Ok U32, BitAnd, Ok U32) -> Ok U32
       | (Ok U32, BitOr, Ok U32) -> Ok U32
       | (Ok U32, BitXor, Ok U32) -> Ok U32

       | _ -> Error "Error: Incorrect types for binary operation"
     end
  | Parsetree.UnOp (op, expr) ->
     Result.bind (eval_expr_type symtab_stack expr)
       begin
         fun t ->
         match (t, op) with
         | (I32, UMinus) -> Ok I32
         | (Bool, Not) -> Ok Bool
         | (I32, BitNot) -> Ok I32
         | (U32, BitNot) -> Ok U32
         | _ -> Error "Error: Incorrect type for unary operation"
       end
  (* TODO *)
  | Parsetree.Index (array, idx) -> Ok I32

let silktype_of_asttype symtab t = match t with
  | Parsetree.I32 -> Ok I32
  | Parsetree.U32 -> Ok U32
  | Parsetree.Void -> Ok Void
  | Parsetree.NewType (name) ->
     match SymtabM.find_opt name symtab with
     | Some (Type t) -> Ok (NewType (name, t))
     | Some (Value _) ->
        Error ("Error: " ^ name ^ " is not a type")
     | None -> Error ("Error: type " ^ name ^ " undefined")

let trav_valdecl symtab symtab_stack types_tab vd =
  let check_inferred_type mut ident expr =
    match SymtabM.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
    | None ->
       Result.map
         (fun stype -> SymtabM.add ident (Value (mut, stype, None)) symtab)
         (eval_expr_type (symtab :: symtab_stack) expr)
  in

  let check_declared_type mut ident asttype expr =
    match SymtabM.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
    | None ->
       let lefttype = silktype_of_asttype types_tab asttype in
       let righttype = eval_expr_type (symtab :: symtab_stack) expr in
       Result.bind righttype
         (fun rstype ->
           Result.bind lefttype (fun lstype ->
               if compare_types lstype rstype then
                 Ok (SymtabM.add ident (Value (mut, lstype, None)) symtab)
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


let rec construct_block_symtab base_symtab symtab_stack types_tab stmts =
  let addblk block_number symtab new_base blk =
    let new_symtab st = SymtabM.add (string_of_int block_number)
                          (Value (Val, Void, Some st))
                          symtab
    in
    Result.map
      (fun st -> (block_number + 1, new_symtab st))
      (construct_block_symtab new_base (symtab :: symtab_stack) types_tab blk)
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
    | Parsetree.Block blk -> addblk block_number symtab SymtabM.empty blk
    | Parsetree.IfElse (exp, ifstmt, elsestmt) ->
       begin
         match ifstmt with
         | Parsetree.Block ifblk ->
            Result.bind (eval_expr_type (symtab :: symtab_stack) exp)
              begin
                fun expr_t ->
                match expr_t with
                | Bool ->
                   let ifresult = addblk block_number symtab SymtabM.empty ifblk in
                   begin
                     match elsestmt with
                     | Parsetree.Block elseblk ->
                        Result.bind
                          ifresult
                          (fun (b, s) -> addblk b s SymtabM.empty elseblk)
                     | Parsetree.Empty -> ifresult
                     | _ -> Error "Error: Not a block"
                   end
                | _ -> Error "Error: Expected boolean expression in 'if' condition"
              end
         | _ -> Error "Error: Not a block"
       end
    | Parsetree.While (exp, whilestmt) ->
       begin
         match whilestmt with
         | Parsetree.Block blk ->
            Result.bind (eval_expr_type (symtab :: symtab_stack) exp)
              begin
                fun expr_t ->
                match expr_t with
                | Bool -> addblk block_number symtab SymtabM.empty blk
                | _ -> Error "Error: Expected boolean expression in 'while' condition"
              end
         | _ -> Error "Error: Not a block"
       end
    | Parsetree.For (vd, condexp, incexp, forblk) ->
       begin
         match forblk with
         | Parsetree.Block blk ->
            let vd_result =
              trav_valdecl SymtabM.empty (symtab :: symtab_stack) types_tab vd
            in
            Result.bind vd_result
              begin
                fun local_symtab ->
                Result.bind
                  (eval_expr_type (local_symtab :: symtab :: symtab_stack) condexp)
                  begin
                    fun condexp_t ->
                    Result.bind
                      (eval_expr_type (local_symtab :: symtab :: symtab_stack) incexp)
                      begin
                        fun incexp_t ->
                        match condexp_t with
                        | Bool -> addblk block_number symtab local_symtab blk
                        | _ ->
                           Error "Error: Expected boolean expression in 'for' condition"
                      end
                  end
              end
         | _ -> Error "Error: Not a block"
       end
    | Parsetree.Return exo ->
       begin
         match exo with
         (* TODO check that return type matches *)
         | Some exp ->
            Result.bind (eval_expr_type (symtab :: symtab_stack) exp)
              begin
                fun expr_t ->
                Ok (block_number, symtab)
              end
         | None -> Ok (block_number, symtab)
       end
    | Parsetree.Continue | Parsetree.Break -> Ok (block_number, symtab)
  in
  Result.map (fun (_, s) -> s) (fold_left_bind trav_stmt (0, base_symtab) stmts)

let construct_symtab ast =
  let trav_funcdecl symtab fd =
    let (ident, arglist, ret_asttype, body) = fd in
    match SymtabM.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
    | None ->
       let define_arg acc argtuple =
         let (new_symtab, argtypes) = acc in
         let (name, asttype) = argtuple in
         Result.bind (silktype_of_asttype symtab asttype)
           begin
             fun argtype ->
             match SymtabM.find_opt name new_symtab with
             | Some _ -> Error ("Error: Duplicate argument " ^ name)
             | None -> Ok (SymtabM.add name (Value (Val, argtype, None)) new_symtab,
                           argtype :: argtypes)
           end
       in
       Result.bind
         (fold_left_bind define_arg (SymtabM.empty, []) arglist)
         begin
           fun arg_result ->
           let (new_symtab, argtypes) = match arg_result with
             | (a, b) -> (a, List.rev b)
           in
           Result.bind (silktype_of_asttype symtab ret_asttype)
             begin
               fun rettype ->
               match body with
               | Parsetree.Block blk ->
                  let nst = SymtabM.add ident
                              (Value (Val, Function (argtypes, rettype), None))
                              symtab
                  in
                  Result.map
                    (fun st -> SymtabM.add ident
                                 (Value (Val, Function (argtypes, rettype), Some st))
                                 symtab)
                    (construct_block_symtab new_symtab [nst] symtab blk)
               | _ -> Error "Error: Not a block"
             end
         end
  in

  let trav_ast symtab decl = match (symtab, decl) with
    | (symtab, Parsetree.TypeDef (ident, basetype)) ->
       begin
         match SymtabM.find_opt ident symtab with
         | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
         | None -> Result.map
                     (fun t -> SymtabM.add ident (Type t) symtab)
                     (silktype_of_asttype symtab basetype)
       end
    | (symtab, ValDecl vd) -> trav_valdecl symtab [] symtab vd
    | (symtab, FuncDecl fd) -> trav_funcdecl symtab fd
  in
  fold_left_bind trav_ast SymtabM.empty ast
