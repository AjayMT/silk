
(*
 * Type checking and symbol table construction.
 *)

module SymtabM = Map.Make(String)

type valness = Val | Var

type silktype = I of int
              | U of int
              | F of int
              | Bool | Void
              | Pointer of silktype | MutPointer of silktype
              | Function of (silktype list) * silktype
              | NewType of string * silktype

type symbol = Value of valness * silktype * symbol SymtabM.t option
            | Type of silktype


let ( let* ) x f = Result.bind x f
let ( let+ ) x f = Result.map f x

let rec fold_left_bind f acc l = match l with
  | [] -> Ok acc
  | (x :: xs) ->
     let* a = f acc x in
     fold_left_bind f a xs

let rec find_symtab_stack name ststack = match ststack with
  | [] -> None
  | (z :: zs) ->
     match SymtabM.find_opt name z with
     | Some v -> Some v
     | None -> find_symtab_stack name zs

let silktype_of_literal_type l = match l with
  | Parsetree.LI8 _  -> I 8
  | Parsetree.LI16 _ -> I 16
  | Parsetree.LI32 _ -> I 32
  | Parsetree.LI64 _ -> I 64
  | Parsetree.LU8 _  -> U 8
  | Parsetree.LU16 _ -> U 16
  | Parsetree.LU32 _ -> U 32
  | Parsetree.LU64 _ -> U 64
  | Parsetree.LF32 _ -> F 32
  | Parsetree.LF64 _ -> F 64
  | Parsetree.LBool _ -> Bool

let rec silktype_of_asttype symtab_stack t = match t with
  | Parsetree.I8  -> Ok (I 8)
  | Parsetree.I16 -> Ok (I 16)
  | Parsetree.I32 -> Ok (I 32)
  | Parsetree.I64 -> Ok (I 64)
  | Parsetree.U8  -> Ok (U 8)
  | Parsetree.U16 -> Ok (U 16)
  | Parsetree.U32 -> Ok (U 32)
  | Parsetree.U64 -> Ok (U 64)
  | Parsetree.F32 -> Ok (F 32)
  | Parsetree.F64 -> Ok (F 64)
  | Parsetree.Bool -> Ok Bool
  | Parsetree.Void -> Ok Void
  | Parsetree.Pointer t ->
     let+ t = silktype_of_asttype symtab_stack t in Pointer t
  | Parsetree.MutPointer t ->
     let+ t = silktype_of_asttype symtab_stack t in MutPointer t
  | Parsetree.Function (ats, rt) ->
     let define_arg acc a =
       let+ a = silktype_of_asttype symtab_stack a in
       a :: acc
     in
     let* args_rev = fold_left_bind define_arg [] ats in
     let args = List.rev args_rev in
     let+ rt = silktype_of_asttype symtab_stack rt in
     Function (args, rt)
  | Parsetree.NewType (name) ->
     match find_symtab_stack name symtab_stack with
     | Some (Type t) -> Ok (NewType (name, t))
     | Some (Value _) ->
        Error ("Error: " ^ name ^ " is not a type")
     | None -> Error ("Error: type " ^ name ^ " undefined")

let rec compare_types a b = match (a, b) with
  | (Function (aargtypes, arettype), Function (bargtypes, brettype)) ->
     let f b t1 t2 = if b then compare_types t1 t2 else b in
     List.length aargtypes = List.length bargtypes
     && (List.fold_left2 f true aargtypes bargtypes)
     && (compare_types arettype brettype)
  | (NewType (aname, atype), NewType (bname, btype)) ->
     (aname = bname) && (compare_types atype btype)
  | (Pointer a, Pointer b) -> compare_types a b
  | (MutPointer a, MutPointer b) -> compare_types a b
  | (a, b) -> a = b

let check_viable_cast cast_t expr_t = match cast_t with
  | I _ | U _ ->
     begin match expr_t with
     | I _ | U _ | F _ | MutPointer _ -> Ok ()
     | _ -> Error "Error: Unviable type cast"
     end
  | F f ->
     begin match expr_t with
     | I _ | U _ | F _ -> Ok ()
     | _ -> Error "Error: Unviable type cast"
     end
  | Pointer _ ->
     begin match expr_t with
     | Pointer _ -> Ok ()
     | _ -> Error "Error: Unviable type cast"
     end
  | MutPointer _ ->
     begin match expr_t with
     | I _ | U _ | MutPointer _ -> Ok ()
     | _ -> Error "Error: Unviable type cast"
     end
  | _ -> if compare_types cast_t expr_t then Ok ()
         else Error "Error: Unviable type cast"

let rec eval_expr_type symtab_stack expr = match expr with
  | Parsetree.Identifier name ->
     begin match find_symtab_stack name symtab_stack with
     | Some (Type _) -> Error ("Error: Expected value, found type: " ^ name)
     | Some (Value (_, t, _)) -> Ok t
     | None -> Error ("Error: Identifier " ^ name ^ " undefined")
     end
  | Parsetree.Literal l -> Ok (silktype_of_literal_type l)
  | Parsetree.Assignment (n, e) ->
     let* exprtype = eval_expr_type symtab_stack e in
     begin match (find_symtab_stack n symtab_stack) with
     | Some Value (Var, idtype, _) ->
        if compare_types idtype exprtype then Ok exprtype
        else Error ("Error: Mismatched types in assignment of " ^ n)
     | Some Value (Val, _, _) -> Error ("Error: Cannot re-assign val " ^ n)
     | Some Type _ -> Error ("Error: Expected value, found type: " ^ n)
     | None -> Error ("Error: Identifier " ^ n ^ " undefined")

     end
  | Parsetree.TypeCast (t, expr) ->
     let* expr_t = eval_expr_type symtab_stack expr in
     let* cast_t = silktype_of_asttype symtab_stack t in
     let+ () = check_viable_cast cast_t expr_t in
     cast_t
  | Parsetree.FunctionCall (f, args) ->
     begin
       let match_arg_types argtypes exprs =
         let match_types acc t exp =
           let* _ = acc in
           let check_arg_type et =
             if compare_types et t then Ok t
             else Error ("Error: Mismatched types in function call")
           in
           Result.bind (eval_expr_type symtab_stack exp) check_arg_type
         in
         if List.length argtypes = List.length args then
           List.fold_left2 match_types (Ok Bool) argtypes exprs
         else Error ("Error: Incorrect number of arguments")
       in
       let check_function_type stype = match stype with
         | Function (argtypes, t) ->
            let+ _ = match_arg_types argtypes args in t
         | _ -> Error ("Error: Expression is not a function")
       in
       Result.bind (eval_expr_type symtab_stack f) check_function_type
     end
  | Parsetree.BinOp (a, op, b) ->
     let* a_type = eval_expr_type symtab_stack a in
     let* b_type = eval_expr_type symtab_stack b in
     let err = Error "Error: Incorrect types for binary operation" in
     begin match op with
     | Plus | Minus ->
        begin match (a_type, b_type) with
        | (I a, I b) | (U a, U b) -> if a = b then Ok a_type else err
        | (Pointer _, I _) | (Pointer _, U _)
          | (MutPointer _, I _) | (MutPointer _, U _) -> Ok a_type
        | _ -> err
        end
     | Times | Divide | Modulus | BitAnd | BitOr | BitXor
       | RShift | LShift ->
        begin match (a_type, b_type) with
        | (I a, I b) | (U a, U b) -> if a = b then Ok a_type else err
        | _ -> err
        end
     | Equal | LessThan | GreaterThan ->
        begin match (a_type, b_type) with
        | (I a, I b) | (U a, U b) -> if a = b then Ok a_type else err
        | (Bool, Bool) | (Pointer _, Pointer _) | (MutPointer _, MutPointer _) ->
           Ok Bool
        | _ -> err
        end
     | And | Or ->
        begin match (a_type, b_type) with
        | (Bool, Bool) -> Ok Bool
        | _ -> err
        end
     end
  | Parsetree.UnOp (op, expr) ->
     let* t = eval_expr_type symtab_stack expr in
     let err = Error "Error: Incorrect type for unary operation" in
     begin match op with
     | Parsetree.UMinus | Parsetree.BitNot ->
        begin match t with
        | I _ | U _ -> Ok t
        | _ -> err
        end
     | Parsetree.Not ->
        begin match t with
        | Bool -> Ok t
        | _ -> err
        end
     | Parsetree.AddressOf ->
        begin match expr with
        | Parsetree.Identifier name ->
           let v = find_symtab_stack name symtab_stack in
           begin match v with
           | Some (Value (Var, _, _)) -> Ok (MutPointer t)
           | Some (Value (Val, _, _)) -> Ok (Pointer t)
           | _ -> err
           end
        | Parsetree.UnOp (Parsetree.Deref, exp) -> eval_expr_type symtab_stack exp
        | _ -> Error "Error: Invalid operand type for 'address' operator"
        end
     | Parsetree.Deref ->
        begin match t with
        | Pointer t -> Ok t
        | MutPointer t -> Ok t
        | _ -> err
        end
     end
  (* TODO *)
  | Parsetree.Index (array, idx) -> Ok (I 32)

let trav_valdecl symtab symtab_stack types_tab vd =
  let check_inferred_type mut ident expr =
    match SymtabM.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
    | None ->
       let+ stype = eval_expr_type (symtab :: symtab_stack) expr in
       SymtabM.add ident (Value (mut, stype, None)) symtab
  in

  let check_declared_type mut ident asttype expr =
    match SymtabM.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
    | None ->
       let* lstype = silktype_of_asttype [types_tab] asttype in
       let* rstype = eval_expr_type (symtab :: symtab_stack) expr in
       if compare_types lstype rstype then
         Ok (SymtabM.add ident (Value (mut, lstype, None)) symtab)
       else
         Error ("Error: mismatched types in declaration of " ^ ident)
  in

  match vd with
  | Parsetree.ValI (ident, expr) -> check_inferred_type Val ident expr
  | Parsetree.Val (ident, asttype, expr) ->
     check_declared_type Val ident asttype expr
  | Parsetree.VarI (ident, expr) -> check_inferred_type Var ident expr
  | Parsetree.Var (ident, asttype, expr) ->
     check_declared_type Var ident asttype expr


let rec construct_block_symtab base_symtab symtab_stack types_tab rettype stmts =
  let addblk block_number symtab new_base blk =
    let new_symtab st = SymtabM.add (string_of_int block_number)
                          (Value (Val, Void, Some st))
                          symtab
    in
    let+ st =
      construct_block_symtab
        new_base (symtab :: symtab_stack) types_tab rettype blk
    in
    (block_number + 1, new_symtab st)
  in

  let trav_stmt acc stmt =
    let (block_number, symtab) = acc in
    match stmt with
    | Parsetree.Empty -> Ok (block_number, symtab)
    | Parsetree.Decl vd ->
       let+ s = trav_valdecl symtab symtab_stack types_tab vd in
       (block_number, s)
    | Parsetree.Expr exp ->
       let+ _ = eval_expr_type (symtab :: symtab_stack) exp in
       (block_number, symtab)
    | Parsetree.Block blk -> addblk block_number symtab SymtabM.empty blk
    | Parsetree.IfElse (exp, ifstmt, elsestmt) ->
       begin match ifstmt with
       | Parsetree.Block ifblk ->
          let* expr_t = eval_expr_type (symtab :: symtab_stack) exp in
          begin match expr_t with
          | Bool ->
             let ifresult = addblk block_number symtab SymtabM.empty ifblk in
             begin match elsestmt with
             | Parsetree.Block elseblk ->
                let* (b, s) = ifresult in
                addblk b s SymtabM.empty elseblk
             | Parsetree.Empty -> ifresult
             | _ -> Error "Error: Not a block"
             end
          | _ -> Error "Error: Expected boolean expression in 'if' condition"

          end
       | _ -> Error "Error: Not a block"
       end
    | Parsetree.While (exp, whilestmt) ->
       begin match whilestmt with
       | Parsetree.Block blk ->
          let* expr_t = eval_expr_type (symtab :: symtab_stack) exp in
          begin match expr_t with
          | Bool -> addblk block_number symtab SymtabM.empty blk
          | _ -> Error "Error: Expected boolean expression in 'while' condition"

          end
       | _ -> Error "Error: Not a block"
       end
    | Parsetree.For (vd, condexp, incexp, forblk) ->
       begin match forblk with
       | Parsetree.Block blk ->
          let* local_symtab =
            trav_valdecl SymtabM.empty (symtab :: symtab_stack) types_tab vd
          in
          let* condexp_t =
            eval_expr_type (local_symtab :: symtab :: symtab_stack) condexp
          in
          let* incexp_t =
            eval_expr_type (local_symtab :: symtab :: symtab_stack) incexp
          in
          begin match condexp_t with
          | Bool -> addblk block_number symtab local_symtab blk
          | _ ->
             Error "Error: Expected boolean expression in 'for' condition"
          end
       | _ -> Error "Error: Not a block"
       end
    | Parsetree.Return exo ->
       begin match exo with
       | Some exp ->
          let* ext = eval_expr_type (symtab :: symtab_stack) exp in
          if compare_types rettype ext then
            Ok (block_number, symtab)
          else Error "Error: Incorrect return type"
       | None ->
          if compare_types rettype Void then
            Ok (block_number, symtab)
          else Error "Error: Incorrect return type"
       end
    | Parsetree.Continue | Parsetree.Break -> Ok (block_number, symtab)
  in
  let+ (_, s) = fold_left_bind trav_stmt (0, base_symtab) stmts in s

let construct_symtab ast =
  let trav_funcdecl symtab (ident, arglist, ret_asttype) =
    let fwd_decl_value = SymtabM.find_opt ident symtab in
    match fwd_decl_value with
    | (Some (Value (Val, Function (_, _), None))) | None ->
       let define_arg acc argtuple =
         let (new_symtab, argtypes) = acc in
         let (name, asttype) = argtuple in
         let* argtype = silktype_of_asttype [symtab] asttype in
         begin match SymtabM.find_opt name new_symtab with
         | Some _ -> Error ("Error: Duplicate argument " ^ name)
         | None -> Ok (SymtabM.add name (Value (Val, argtype, None)) new_symtab,
                       argtype :: argtypes)
         end
       in
       let* (new_symtab, argtypes_r) =
         fold_left_bind define_arg (SymtabM.empty, []) arglist
       in
       let argtypes = List.rev argtypes_r in
       let* rettype = silktype_of_asttype [symtab] ret_asttype in
       let func_t = Function (argtypes, rettype) in
       begin match fwd_decl_value with
       | Some (Value (Val, fwd_decl_t, None)) ->
          if compare_types fwd_decl_t func_t then
            Ok (new_symtab, func_t)
          else Error ("Error: Types of " ^ ident ^ " do not match")
       | None -> Ok (new_symtab, func_t)
       | _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
       end
    | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
  in

  let trav_ast symtab decl = match (symtab, decl) with
    | (symtab, Parsetree.TypeDef (ident, basetype)) ->
       begin match SymtabM.find_opt ident symtab with
       | Some _ -> Error ("Error: Symbol " ^ ident ^ " already defined")
       | None ->
          let+ t = silktype_of_asttype [symtab] basetype in
          SymtabM.add ident (Type t) symtab
       end
    | (symtab, ValDecl vd) -> trav_valdecl symtab [] symtab vd
    | (symtab, FuncDecl (ident, arglist, ret_asttype, body)) ->
       let* (new_symtab, ft) =
         trav_funcdecl symtab (ident, arglist, ret_asttype)
       in
       begin match body with
       | Parsetree.Block blk ->
          let nst = SymtabM.add ident (Value (Val, ft, None)) symtab in
          let rt = match ft with
            | Function (_, rt) -> rt
            | _ -> Void
          in
          let+ st = construct_block_symtab new_symtab [nst] symtab rt blk in
          SymtabM.add ident (Value (Val, ft, Some st)) symtab
       | _ -> Error "Error: Not a block"
       end
    | (symtab, FuncFwdDecl (ident, arglist, ret_asttype, _)) ->
       let+ (_, ft) = trav_funcdecl symtab (ident, arglist, ret_asttype) in
       SymtabM.add ident (Value (Val, ft, None)) symtab
  in
  fold_left_bind trav_ast SymtabM.empty ast
