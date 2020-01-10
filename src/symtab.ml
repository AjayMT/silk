
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
              | Array of int * silktype
              | Struct of bool * silktype list
              | StructLabeled of bool * (string * silktype) list
              | Function of (silktype list) * silktype
              | TypeAlias of string * silktype

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
  | Parsetree.LString _ -> Pointer (I 8)

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
  | Parsetree.Array (len, t) ->
     let+ t = silktype_of_asttype symtab_stack t in Array (len, t)
  | Parsetree.Struct (packed, types) ->
     let add_type acc t =
       let+ t = silktype_of_asttype symtab_stack t in
       t :: acc
     in
     let+ types = fold_left_bind add_type [] types in
     Struct (packed, List.rev types)
  | Parsetree.StructLabeled (packed, members) ->
     let (names, types) = List.split members in
     let add_type acc t =
       let+ t = silktype_of_asttype symtab_stack t in
       t :: acc
     in
     let+ types = fold_left_bind add_type [] types in
     StructLabeled (packed, List.combine names (List.rev types))
  | Parsetree.Function (ats, rt) ->
     let define_arg acc a =
       let+ a = silktype_of_asttype symtab_stack a in
       a :: acc
     in
     let* args_rev = fold_left_bind define_arg [] ats in
     let args = List.rev args_rev in
     let+ rt = silktype_of_asttype symtab_stack rt in
     Function (args, rt)
  | Parsetree.TypeAlias (name) ->
     match find_symtab_stack name symtab_stack with
     | Some (Type t) -> Ok (TypeAlias (name, t))
     | Some (Value _) ->
        Error ("Error: " ^ name ^ " is not a type")
     | None -> Error ("Error: type " ^ name ^ " undefined")

let rec resolve_type_alias t = match t with
  | TypeAlias (_, t) -> resolve_type_alias t
  | _ -> t

let rec compare_types a b = match (a, b) with
  | (Function (aargtypes, arettype), Function (bargtypes, brettype)) ->
     let f b t1 t2 = if b then compare_types t1 t2 else b in
     List.length aargtypes = List.length bargtypes
     && (List.fold_left2 f true aargtypes bargtypes)
     && (compare_types arettype brettype)
  | (TypeAlias (_, a), b) -> compare_types (resolve_type_alias a) b
  | (a, TypeAlias (_, b)) -> compare_types a (resolve_type_alias b)
  | (Pointer a, Pointer b) -> compare_types a b
  | (MutPointer a, MutPointer b) -> compare_types a b
  | (Array (l1, t1), Array (l2, t2)) -> (l1 = l2) && (compare_types t1 t2)
  | (Struct (p1, a), Struct (p2, b)) ->
     p1 == p2 &&
       if List.length a = List.length b then
         let results =
           List.map (fun (a, b) -> compare_types a b) (List.combine a b)
         in
         List.fold_left (&&) true results
       else false
  | (StructLabeled (p1, a), StructLabeled (p2, b)) ->
     p1 = p2 &&
       if List.length a = List.length b then
         let results =
           List.map (fun ((an, at), (bn, bt)) -> (compare_types at bt) && an = bn)
             (List.combine a b)
         in
         List.fold_left (&&) true results
       else false
  | (a, b) -> a = b

let rec check_valid_cast cast_t expr_t =
  let err = Error "Error: Invalid type cast" in
  match cast_t with
  | I _ | U _ ->
     begin match expr_t with
     | I _ | U _ | F _ | MutPointer _ -> Ok ()
     | _ -> err
     end
  | F f ->
     begin match expr_t with
     | I _ | U _ | F _ -> Ok ()
     | _ -> err
     end
  | Pointer _ ->
     begin match expr_t with
     | Pointer _ -> Ok ()
     | _ -> err
     end
  | MutPointer _ ->
     begin match expr_t with
     | I _ | U _ | MutPointer _ -> Ok ()
     | _ -> err
     end
  | Struct (_, a) ->
     begin match expr_t with
     | StructLabeled (p, b) ->
        let s = Struct (p, List.map (fun (_, t) -> t) b) in
        if compare_types cast_t s then Ok () else err
     | _ ->
        if List.length a = 1 then
          if compare_types (List.hd a) expr_t then Ok ()
          else err
        else
          if compare_types cast_t expr_t then Ok ()
          else err
     end
  | StructLabeled (p, a) ->
     begin match expr_t with
     | Struct (_, b) ->
        let s = Struct (p, List.map (fun (_, t) -> t) a) in
        if compare_types expr_t s then Ok () else err
     | _ ->
        if List.length a = 1 then
          if compare_types ((fun (_, t) -> t) (List.hd a)) expr_t then Ok ()
          else err
        else
          if compare_types cast_t expr_t then Ok ()
          else err
     end
  | TypeAlias (_, a) -> check_valid_cast a expr_t
  | _ -> if compare_types cast_t expr_t then Ok ()
         else err

let rec eval_expr_type symtab_stack expr =
  let rec check_mutable exp = match exp with
    | Parsetree.Identifier name ->
       let v = find_symtab_stack name symtab_stack in
       begin match v with
       | Some (Value (Var, _, _)) -> Ok true
       | Some (Value (Val, _, _)) -> Ok false
       | _ -> Error ("Error: Expected value, found type: " ^ name)
       end
    | Parsetree.UnOp (Parsetree.Deref, exp) ->
       let* t = eval_expr_type symtab_stack exp in
       begin match t with
       | MutPointer _ -> Ok true
       | Pointer _ -> Ok false
       | _ -> Error "Error: Incorrect type for unary operation"
       end
    | Parsetree.Index (array, _) -> check_mutable array
    | Parsetree.StructIndexAccess (struct_expr, _)
      | Parsetree.StructMemberAccess (struct_expr, _) -> check_mutable struct_expr
    | _ -> Error "Error: Cannot get address of temporary value"
  in

  match expr with
  | Parsetree.Identifier name ->
     begin match find_symtab_stack name symtab_stack with
     | Some (Type _) -> Error ("Error: Expected value, found type: " ^ name)
     | Some (Value (_, t, _)) -> Ok t
     | None -> Error ("Error: Identifier " ^ name ^ " undefined")
     end
  | Parsetree.Literal l -> Ok (silktype_of_literal_type l)
  | Parsetree.Assignment (lval, e) ->
     let* exprtype = eval_expr_type symtab_stack e in
     let rec check_lval lval expected_type =
       let* lval_type = eval_expr_type symtab_stack lval in
       match lval with
       | Parsetree.StructLiteral (_, exprs) ->
          let err = Error "Error: Mismatched types in struct assignment" in
          let add_lval acc lv et =
            let* acc = acc in
            let+ t = check_lval lv et in t
          in
          let* valtypes = match expected_type with
            | Struct (_, l) -> if List.length exprs <> List.length l then err
                               else Ok l
            | _ -> err
          in
          let* lvals = List.fold_left2 add_lval (Ok expected_type) exprs valtypes in
          if compare_types lval_type expected_type then Ok expected_type
          else err
       | _ ->
          let* (err1, err2) = match lval with
            | Parsetree.Identifier (n) ->
               Ok ("assignment of " ^ n, "re-assign val " ^ n)
            | Parsetree.UnOp (Parsetree.Deref, _) ->
               Ok ("pointer assignment", "write immutable pointer")
            | Parsetree.Index (_, _) ->
               Ok ("array index assignment", "write immutable array")
            | Parsetree.StructIndexAccess (_, _)
              | Parsetree.StructMemberAccess (_, _) ->
               Ok ("struct member assignment", "write immutable struct")
            | _ -> Error "Error: Invalid lvalue expression"
          in
          let* mut = check_mutable lval in
          if mut then
            if compare_types lval_type expected_type then Ok expected_type
            else Error ("Error: Mismatched types in " ^ err1)
          else
            Error ("Error: Cannot " ^ err2)
     in
     check_lval lval exprtype
  | Parsetree.TypeCast (t, expr) ->
     let* expr_t = eval_expr_type symtab_stack expr in
     let* cast_t = silktype_of_asttype symtab_stack t in
     let+ () = check_valid_cast cast_t expr_t in
     cast_t
  | Parsetree.FunctionCall (f, args) ->
     let match_arg_types argtypes exprs =
       let match_types acc t exp =
         let* _ = acc in
         let check_arg_type et =
           let (a, b) = match (t, et) with
             | (Pointer a, MutPointer b) -> (a, b)
             | (a, b) -> (a, b) in
           if compare_types a b then Ok t
           else Error "Error: Mismatched types in function call"
         in
         Result.bind (eval_expr_type symtab_stack exp) check_arg_type
       in
       if List.length argtypes = List.length args then
         List.fold_left2 match_types (Ok Bool) argtypes exprs
       else Error "Error: Incorrect number of arguments"
     in
     let check_function_type stype = match stype with
       | Function (argtypes, t) ->
          let+ _ = match_arg_types argtypes args in t
       | _ -> Error "Error: Expression is not a function"
     in
     begin match (eval_expr_type symtab_stack f) with
     | Ok t -> check_function_type t
     | Error e ->
        begin match f with
        | Parsetree.Identifier t ->
           if List.length args <> 1 then
             Error "Error: Invalid type cast expression"
           else eval_expr_type symtab_stack
                  (Parsetree.TypeCast (Parsetree.TypeAlias t, List.hd args))
        | _ -> Error e
        end
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
        | Parsetree.Index (array, _) ->
           let+ mut = check_mutable array in
           if mut then MutPointer t else Pointer t
        | Parsetree.StructIndexAccess (exp, _)
          | Parsetree.StructMemberAccess (exp, _) ->
           let+ mut = check_mutable exp in
           if mut then MutPointer t else Pointer t
        | _ -> Error "Error: Cannot get address of temporary value"
        end
     | Parsetree.Deref ->
        begin match t with
        | Pointer t -> Ok t
        | MutPointer t -> Ok t
        | _ -> err
        end
     end

  | Parsetree.StructLiteral (packed, exprs) ->
     let add_expr acc expr =
       let+ t = eval_expr_type symtab_stack expr in
       t :: acc
     in
     let+ types = fold_left_bind add_expr [] exprs in
     Struct (packed, List.rev types)

  | Parsetree.ArrayElems [] -> Error "Error: Empty array initializer"
  | Parsetree.ArrayElems (head :: tail) ->
     let check_elem acc elem =
       let* elem_type = eval_expr_type symtab_stack elem in
       if compare_types acc elem_type then Ok elem_type
       else Error "Error: Mismatched types in array initializer"
     in
     let* head_type = eval_expr_type symtab_stack head in
     let+ elem_type = fold_left_bind check_elem head_type tail in
     Array (List.length (head :: tail), elem_type)
  | Parsetree.ArrayInit (t, len) ->
     let* t = silktype_of_asttype symtab_stack t in
     if len < 0 then Error "Error: Negative array length"
     else Ok (Array (len, t))
  | Parsetree.Index (array, idx) ->
     let* array_type = eval_expr_type symtab_stack array in
     let* idx_type = eval_expr_type symtab_stack idx in
     begin match idx_type with
     | I _ | U _ ->
        begin match array_type with
        | Array (_, elem_type) -> Ok elem_type
        | _ -> Error "Error: Cannot index non-array type"
        end
     | _ -> Error "Error: Array index must be integer type"
     end
  | Parsetree.StructMemberAccess (exp, name) ->
     let* t = eval_expr_type symtab_stack exp in
     let* members = match (resolve_type_alias t) with
       | StructLabeled (_, l) -> Ok l
       | _ -> Error "Error: Cannot access member of non-labeledstruct type"
     in
     begin match List.assoc_opt name members with
     | Some t -> Ok t
     | None -> Error ("Error: Struct has no member named " ^ name)
     end
  | Parsetree.StructIndexAccess (exp, idx) ->
     let* t = eval_expr_type symtab_stack exp in
     let* members = match (resolve_type_alias t) with
       | Struct (_, l) -> Ok l
       | _ -> Error "Error: Cannot access member of non-unlabeledstruct type"
     in
     match List.nth_opt members idx with
     | Some t -> Ok t
     | None -> Error ("Error: Struct has no member at index " ^ (string_of_int idx))

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
