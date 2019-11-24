
(*
 * LLVM Code generation.
 *)

module ScopeM = Map.Make(String)

type llvm_type = I1 | I8 | I16 | I32 | I64
                 | U1 | U8 | U16 | U32 | U64
                 | F32 | F64
                 | Pointer of llvm_type
                 | Function of llvm_type list * llvm_type
                 | Void

type ir_literal = Int of int | Float of float
type ir_bin_op = Plus | Minus | Times | Divide | Modulus
                 | Equal | LessThan | GreaterThan
                 | And | Or
                 | RShift | LShift | Xor
type ir_un_op = UMinus | Not
type ir_expr = Identifier of llvm_type * string
             | ParamIdentifier of llvm_type * string
             | Literal of llvm_type * ir_literal
             | Assignment of llvm_type * string * ir_expr
             | FunctionCall of llvm_type * ir_expr * llvm_type list * ir_expr list
             | BinOp of llvm_type * ir_bin_op * ir_expr * ir_expr
             | UnOp of llvm_type * ir_un_op * ir_expr
             | ItoF of llvm_type * ir_expr * llvm_type
             | FtoI of llvm_type * ir_expr * llvm_type
             | BitCast of llvm_type * ir_expr * llvm_type
type ir_decl = llvm_type * string * ir_expr
type ir_stmt = Empty
             | Decl of ir_decl
             | Expr of ir_expr
             | Block of string * ir_stmt list
             | IfElse of string * string * ir_expr * ir_stmt list * ir_stmt list
             | While of string * ir_expr * ir_stmt list
             | For of string * ir_decl * ir_expr * ir_expr * ir_stmt list
             | Continue
             | Break
             | Return of ir_expr option
type ir_root = StaticDecl of llvm_type * string * ir_literal
             | FuncDecl of llvm_type * string
                           * (llvm_type * string) list * ir_stmt list
             | FuncFwdDecl of llvm_type * string
                              * (llvm_type * string) list * bool

type llvm_value = LTemporary of int
                | LNamed of string
                | LLiteral of ir_literal
                | NoValue
type llvm_inst = Alloca of llvm_type
               | Load of llvm_type * llvm_type * llvm_value
               | Store of llvm_type * llvm_value * llvm_type * llvm_value
               | Call of llvm_type * llvm_value * (llvm_type * llvm_value) list
               | Ret of llvm_type * llvm_value
               | Label of string
               | BranchCond of llvm_value * string * string
               | Branch of string

               | Add of llvm_type * llvm_value * llvm_value
               | Sub of llvm_type * llvm_value * llvm_value
               | Mul of llvm_type * llvm_value * llvm_value
               | Div of llvm_type * llvm_value * llvm_value
               | Rem of llvm_type * llvm_value * llvm_value
               | FNeg of llvm_type * llvm_value

               | CmpEq of llvm_type * llvm_value * llvm_value
               | CmpLt of llvm_type * llvm_value * llvm_value
               | CmpGt of llvm_type * llvm_value * llvm_value

               | And of llvm_type * llvm_value * llvm_value
               | Or of llvm_type * llvm_value * llvm_value
               | Xor of llvm_type * llvm_value * llvm_value

               | Shl of llvm_type * llvm_value * llvm_value
               | Shr of llvm_type * llvm_value * llvm_value

               | ItoFP of llvm_type * llvm_value * llvm_type
               | FPtoI of llvm_type * llvm_value * llvm_type
               | BitCast of llvm_type * llvm_value * llvm_type

               | NoInst


let ( let* ) x f = Result.bind x f
let ( let+ ) x f = Result.map f x

let rec llvm_type_of_silktype t =
  match t with
  | Symtab.Function (argtypes, rettype) ->
     let llargtypes = List.map llvm_type_of_silktype argtypes in
     let llrettype = llvm_type_of_silktype rettype in
     Function (llargtypes, llrettype)
  | Symtab.Bool -> I1
  | Symtab.I32 -> I32
  | Symtab.U32 -> U32
  | Symtab.F64 -> F64
  | Symtab.Void -> Void
  | Symtab.NewType (_, t) -> llvm_type_of_silktype t

(* TODO binops, other things *)
let resolve_literal l = match l with
  | Parsetree.Literal l -> begin
      match l with
      | Parsetree.LI32 i -> Ok (Int i)
      | Parsetree.LU32 i -> Ok (Int i)
      | Parsetree.LF64 f -> Ok (Float f)
      | Parsetree.LBool b -> Ok (Int (if b then 1 else 0))
    end
  | _ -> Error "Error: Could not resolve static value at compile time"

let get_ir_expr_type ir_exp = match ir_exp with
  | Identifier (t, _) -> t
  | ParamIdentifier (t, _) -> t
  | Literal (t, _) -> t
  | Assignment (t, _, _) -> t
  | FunctionCall (t, _, _, _) -> t
  | BinOp (t, _, _, _) -> t
  | UnOp (t, _, _) -> t
  | ItoF (_, _, t) -> t
  | FtoI (_, _, t) -> t
  | BitCast (_, _, t) -> t

let construct_ir_tree ast symtab =
  let rec find_in_scope scope_stack symtab_stack name =
    match symtab_stack with
    | [] -> ([], [], Symtab.SymtabM.find name symtab)
    | z :: zs ->
       match Symtab.SymtabM.find_opt name z with
       | Some v -> (scope_stack, symtab_stack, v)
       | None -> find_in_scope (List.tl scope_stack) zs name
  in

  let rec find_symtab_stack name symtab_stack =
    match symtab_stack with
    | [] -> Symtab.SymtabM.find_opt name symtab
    | (z :: zs) ->
       match Symtab.SymtabM.find_opt name z with
       | Some v -> Some v
       | None -> find_symtab_stack name zs
  in

  let rec map_expr scope_map symtab_stack expr =
    match expr with
    | Parsetree.Identifier name ->
       begin
         let symbol = find_symtab_stack name symtab_stack in
         match symbol with
         | Some (Symtab.Type _) -> Error ("Error: Expected value, found type: " ^ name)
         | Some (Symtab.Value (_, type_, _)) ->
            let t = llvm_type_of_silktype type_ in
            Ok (Identifier (t, ScopeM.find name scope_map))
         | None -> Error ("Error: Identifier " ^ name ^ " undefined")
       end
    | Parsetree.Literal l ->
       begin
         match l with
         | Parsetree.LI32 i -> Ok (Literal (I32, Int i))
         | Parsetree.LU32 i -> Ok (Literal (U32, Int i))
         | Parsetree.LF64 f -> Ok (Literal (F64, Float f))
         | Parsetree.LBool b -> Ok (Literal (I1, Int (if b then 1 else 0)))
       end
    | Parsetree.Assignment (name, exp) ->
       begin
         let symbol = find_symtab_stack name symtab_stack in
         match symbol with
         | Some (Symtab.Type _) -> Error ("Error: Expected value, found type: " ^ name)
         | Some (Symtab.Value (_, type_, _)) ->
            let t = llvm_type_of_silktype type_ in
            let+ ir_exp = map_expr scope_map symtab_stack exp in
            Assignment (t, ScopeM.find name scope_map, ir_exp)
         | None -> Error ("Error: Identifier " ^ name ^ " undefined")
       end
    | Parsetree.TypeCast (type_, expr) ->
       let* silktype = Symtab.silktype_of_asttype symtab_stack type_ in
       let* ir_expr = map_expr scope_map symtab_stack expr in
       let casttype = llvm_type_of_silktype silktype in
       let ir_expr_type = get_ir_expr_type ir_expr in
       (* TODO extend, truncate *)
       begin
         match casttype with
         | F64 | F32 ->
            begin
              match ir_expr_type with
              | I1 | I8 | I16 | I32 | I64 | U1 | U8 | U16 | U32 | U64 ->
                 Ok (ItoF (ir_expr_type, ir_expr, casttype))
              | _ -> Ok (BitCast (ir_expr_type, ir_expr, casttype))
            end
         | I1 | I8 | I16 | I32 | I64 | U1 | U8 | U16 | U32 | U64 ->
            begin
              match ir_expr_type with
              | F32 | F64 ->
                 Ok (FtoI (ir_expr_type, ir_expr, casttype))
              | _ ->
                 Ok (BitCast (ir_expr_type, ir_expr, casttype))
            end
         | _ -> Ok (BitCast (ir_expr_type, ir_expr, casttype))
       end
    | Parsetree.FunctionCall (fexp, argexps) ->
       let* ir_fexp = map_expr scope_map symtab_stack fexp in
       begin
         match (get_ir_expr_type ir_fexp) with
         | Function (argtypes, rettype) ->
            let add_arg args ar_exp =
              let+ ir_argexp = map_expr scope_map symtab_stack ar_exp in
              ir_argexp :: args
            in
            let+ args = Symtab.fold_left_bind add_arg [] argexps in
            FunctionCall (rettype, ir_fexp, argtypes, args)
         | _ -> Error "Error: Not a function"
       end
    | Parsetree.BinOp (lexp_, op_, rexp_) ->
       begin
         let lexp = map_expr scope_map symtab_stack lexp_ in
         let rexp = map_expr scope_map symtab_stack rexp_ in
         match (lexp, rexp) with
         | (Error e, _) -> Error e
         | (_, Error e) -> Error e
         | (Ok l, Ok r) ->
            match op_ with
            | Parsetree.Plus -> Ok (BinOp (get_ir_expr_type l, Plus, l, r))
            | Parsetree.Minus -> Ok (BinOp (get_ir_expr_type l, Minus, l, r))
            | Parsetree.Times -> Ok (BinOp (get_ir_expr_type l, Times, l, r))
            | Parsetree.Divide -> Ok (BinOp (get_ir_expr_type l, Divide, l, r))
            | Parsetree.Modulus -> Ok (BinOp (get_ir_expr_type l, Modulus, l, r))

            | Parsetree.Equal -> Ok (BinOp (get_ir_expr_type l, Equal, l, r))
            | Parsetree.LessThan -> Ok (BinOp (get_ir_expr_type l, LessThan, l, r))
            | Parsetree.GreaterThan ->
               Ok (BinOp (get_ir_expr_type l, GreaterThan, l, r))

            | Parsetree.And | Parsetree.BitAnd ->
               Ok (BinOp (get_ir_expr_type l, And, l, r))
            | Parsetree.Or | Parsetree.BitOr ->
               Ok (BinOp (get_ir_expr_type l, Or, l, r))
            | Parsetree.BitXor -> Ok (BinOp (get_ir_expr_type l, Xor, l, r))

            | Parsetree.LShift -> Ok (BinOp (get_ir_expr_type l, LShift, l, r))
            | Parsetree.RShift -> Ok (BinOp (get_ir_expr_type l, RShift, l, r))
       end
    | Parsetree.UnOp (op, expr) ->
       let+ ir_expr = map_expr scope_map symtab_stack expr in
       let o = match op with
         | Parsetree.UMinus -> UMinus
         | Parsetree.Not | Parsetree.BitNot -> Not
       in
       UnOp (get_ir_expr_type ir_expr, o, ir_expr)
    (* TODO *)
    | Parsetree.Index (array_exp, _) -> map_expr scope_map symtab_stack
                                          array_exp
  in

  let rec map_stmt acc stmt =
    let blk_name i scopes =
      (String.concat "." (List.rev scopes)) ^ "." ^ (string_of_int i)
    in

    let map_blk blk acc =
      let (block_idx, scope_stack, scope_map, symtab_stack, ir_stmts) = acc in
      match Symtab.SymtabM.find (string_of_int block_idx) (List.hd symtab_stack) with
      | Symtab.Value (_, _, Some new_symtab) ->
         (map_stmts
            blk
            ((string_of_int block_idx) :: scope_stack)
            scope_map
            (new_symtab :: symtab_stack))
      | _ -> Error "Error: Not a block"
    in

    let map_decl vd acc =
      let (block_idx, scope_stack, scope_map, symtab_stack, ir_stmts) = acc in
      let (name, expr) = match vd with
        | Parsetree.ValI (n, e)    -> (n, e)
        | Parsetree.Val  (n, _, e) -> (n, e)
        | Parsetree.VarI (n, e)    -> (n, e)
        | Parsetree.Var  (n, _, e) -> (n, e)
      in
      let result = find_in_scope scope_stack symtab_stack name in
      match result with
      | (_, _, Symtab.Type _) ->
         Error ("Error: Expected value, found type: " ^ name)
      | (scopes, _, Symtab.Value (_, type_, _)) ->
         let t = llvm_type_of_silktype type_ in
         let prefix = String.concat "." (List.rev scopes) in
         let resolved_name = "%" ^ prefix ^ "." ^ name in
         let+ ir_exp = map_expr scope_map symtab_stack expr in
         (t, name, resolved_name, ir_exp)
    in

    let (block_idx, scope_stack, scope_map, symtab_stack, ir_stmts) = acc in
    match stmt with
    | Parsetree.Empty -> Ok acc
    | Parsetree.Decl vd ->
       let+ (t, name, resolved_name, exp) = map_decl vd acc in
       let new_stmt = Decl (t, resolved_name, exp) in
       (block_idx, scope_stack, (ScopeM.add name resolved_name scope_map),
        symtab_stack, new_stmt :: ir_stmts)
    | Parsetree.Expr expr ->
       let+ ir_exp = map_expr scope_map symtab_stack expr in
       (block_idx, scope_stack, scope_map,
        symtab_stack, (Expr ir_exp) :: ir_stmts)
    | Parsetree.Block blk ->
       let+ stmts = map_blk (List.rev blk) acc in
       (block_idx + 1, scope_stack, scope_map, symtab_stack,
        Block (blk_name block_idx scope_stack, stmts)
        :: ir_stmts)
    | Parsetree.IfElse (cond_expr, ifblock, elseblock) ->
       begin
         match (ifblock, elseblock) with
         | (Parsetree.Block ifstmts, Parsetree.Block elsestmts) ->
            let* cond_ir_exp = map_expr scope_map symtab_stack cond_expr in
            let* if_ir_stmts = map_blk (List.rev ifstmts) acc in
            let acc = (block_idx + 1, scope_stack,
                       scope_map, symtab_stack, ir_stmts) in
            let+ else_ir_stmts = map_blk (List.rev elsestmts) acc in
            let new_stmt = IfElse (blk_name block_idx scope_stack,
                                   blk_name (block_idx + 1) scope_stack,
                                   cond_ir_exp, if_ir_stmts,
                                   else_ir_stmts) in
            (block_idx + 2, scope_stack, scope_map, symtab_stack,
             new_stmt :: ir_stmts)
         | _ -> Error "Error: Not a block"
       end
    | Parsetree.While (cond_expr, whileblock) ->
       begin
         match whileblock with
         | Parsetree.Block whilestmts ->
            let* cond_ir_exp = map_expr scope_map symtab_stack cond_expr in
            let+ while_ir_stmts = map_blk (List.rev whilestmts) acc in
            let new_stmt = While (blk_name block_idx scope_stack,
                                  cond_ir_exp, while_ir_stmts) in
            (block_idx + 1, scope_stack, scope_map,
             symtab_stack, new_stmt :: ir_stmts)
         | _ -> Error "Error: Not a block"
       end
    | Parsetree.For (vd, cond_expr, inc_expr, forblock) ->
       begin
         match forblock with
         | Parsetree.Block forstmts ->
            let new_symtab = match Symtab.SymtabM.find
                                     (string_of_int block_idx)
                                     (List.hd symtab_stack) with
              | Symtab.Value (_, _, Some new_symtab) -> new_symtab
              | _ -> Symtab.SymtabM.empty
            in
            let new_symtab_stack = new_symtab :: symtab_stack in
            let new_scope_stack = (string_of_int block_idx) :: scope_stack in
            let new_acc = (block_idx, new_scope_stack, scope_map, new_symtab_stack,
                           ir_stmts) in
            let* (t, name, resolved_name, exp) = map_decl vd new_acc in
            let (block_idx, scope_stack, scope_map,
                 symtab_stack, ir_stmts) = acc in
            let scope_map = ScopeM.add name resolved_name scope_map in
            let acc = (block_idx, scope_stack, scope_map,
                       symtab_stack, ir_stmts) in
            let* cond_ir_exp = map_expr scope_map new_symtab_stack cond_expr in
            let* inc_ir_exp = map_expr scope_map new_symtab_stack inc_expr in
            let+ for_ir_stmts = map_blk (List.rev forstmts) acc in
            let new_stmt = For (blk_name block_idx scope_stack,
                                (t, resolved_name, exp),
                                cond_ir_exp, inc_ir_exp,
                                for_ir_stmts) in
            (block_idx + 1, scope_stack, scope_map, symtab_stack,
             new_stmt :: ir_stmts)
         | _ -> Error "Error: Not a block"
       end
    | Parsetree.Continue ->
       Ok (block_idx, scope_stack, scope_map, symtab_stack, Continue :: ir_stmts)
    | Parsetree.Break ->
       Ok (block_idx, scope_stack, scope_map, symtab_stack, Break :: ir_stmts)
    | Parsetree.Return ret_exp_opt ->
       let ret_ir_exp_opt =
         Option.map
           (map_expr scope_map symtab_stack)
           ret_exp_opt
       in
       let new_stmt_res = match ret_ir_exp_opt with
         | Some (Error e) -> Error e
         | Some (Ok ret_expr) -> Ok (Return (Some ret_expr))
         | None -> Ok (Return None)
       in
       let+ new_stmt = new_stmt_res in
       (block_idx, scope_stack, scope_map, symtab_stack, new_stmt :: ir_stmts)

  and map_stmts stmts scope_stack scope_map symtab_stack =
    let+ (_, _, _, _, ir_stmts) =
      Symtab.fold_left_bind map_stmt (0, scope_stack, scope_map,
                                      symtab_stack, []) stmts
    in
    ir_stmts
  in

  let map_top_decl acc td =
    let (scope_map, decls) = acc in
    match td with
    | Parsetree.TypeDef _ -> Ok acc
    | Parsetree.ValDecl vd ->
       begin
         let (name, expr) = match vd with
           | Parsetree.ValI (n, e)    -> (n, e)
           | Parsetree.Val  (n, _, e) -> (n, e)
           | Parsetree.VarI (n, e)    -> (n, e)
           | Parsetree.Var  (n, _, e) -> (n, e)
         in
         match (Symtab.SymtabM.find name symtab) with
         | Symtab.Type _ -> Error ("Error: Expected value, found type: " ^ name)
         | Symtab.Value (_, type_, _) ->
            let t = llvm_type_of_silktype type_ in
            let resolved_name = "@" ^ name in
            let+ l = resolve_literal expr in
            (ScopeM.add name resolved_name scope_map,
             (StaticDecl (t, resolved_name, l)) :: decls)
       end
    | Parsetree.FuncDecl (name, args_, _, body) ->
       begin
         let value_ = Symtab.SymtabM.find name symtab in
         match value_ with
         | Symtab.Type _ -> Error ("Error: Expected value, found type: " ^ name)
         | Symtab.Value (_, Symtab.Function (argtypes, rettype), inner_st) ->
            let rt = llvm_type_of_silktype rettype in
            let silktyped_args = List.map
                                   (fun ((n, _), t) -> (n, t))
                                   (List.combine args_ argtypes) in
            let decl_of_arg = fun (n, t) ->
              let lt = llvm_type_of_silktype t in
              Decl (lt, "%" ^ name ^ "." ^ n, ParamIdentifier (lt, "%" ^ n)) in
            let arg_decl_stmts = List.map decl_of_arg silktyped_args in
            let args = List.map
                         (fun (n, t) -> (llvm_type_of_silktype t, "%" ^ n))
                         silktyped_args in
            begin
              match body with
              | Parsetree.Block stmts ->
                 begin
                   let resolved_name = "@" ^ name in
                   let scope_map_ = ScopeM.add name resolved_name scope_map in
                   let scope_map =
                     List.fold_left
                       (fun sm (n, _) ->
                         ScopeM.add n ("%" ^ name ^ "." ^ n) sm)
                       scope_map_ silktyped_args in
                   let+ ir_stmts =
                     map_stmts stmts [name] scope_map [Option.get inner_st]
                   in
                   let ir_stmts =
                     if rt == Void then
                       (Return None) :: ir_stmts
                     else ir_stmts
                   in
                   (scope_map_,
                    (FuncDecl (rt, resolved_name, args,
                               ir_stmts @ arg_decl_stmts)) :: decls)
                 end
              | _ -> Error "Error: Function body is not a block"
            end
         | _ -> Error ("Error: Symbol " ^ name ^ " is not a function")
       end
    | Parsetree.FuncFwdDecl (name, args_, _, extern) ->
       begin
         let value_ = Symtab.SymtabM.find name symtab in
         match value_ with
         | Symtab.Type _ -> Error ("Error: Expected value, found type: " ^ name)
         | Symtab.Value (_, Symtab.Function (argtypes, rettype), _) ->
            let silktyped_args = List.map
                                   (fun ((n, _), t) -> (n, t))
                                   (List.combine args_ argtypes) in
            let args = List.map
                         (fun (n, t) -> (llvm_type_of_silktype t, "%" ^ n))
                         silktyped_args in
            let resolved_name = "@" ^ name in
            let scope_map = ScopeM.add name resolved_name scope_map in
            let rt = llvm_type_of_silktype rettype in
            Ok (scope_map,
                (FuncFwdDecl
                   (rt, resolved_name, args, extern)) :: decls)
         | _ -> Error ("Error: Symbol " ^ name ^ " is not a function")
       end
  in
  let+ (_, l) = Symtab.fold_left_bind map_top_decl (ScopeM.empty, []) ast in
  List.rev l

let rec codegen_expr acc expr =
  let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
  match expr with
  | Identifier (t, name) ->
     begin
       match t with
       | Function (_, _) ->
          Ok (cont_label, brk_label, tmp_idx, insts, LNamed name)
       | _ ->
          let res = LTemporary tmp_idx in
          let new_inst = (res, Load (t, Pointer t, LNamed name)) in
          Ok (cont_label, brk_label, tmp_idx + 1, new_inst :: insts, res)
     end
  | ParamIdentifier (t, name) ->
     Ok (cont_label, brk_label, tmp_idx, insts, LNamed name)
  | Literal (t, lit) ->
     Ok (cont_label, brk_label, tmp_idx, insts, LLiteral lit)
  | Assignment (t, name, rexpr) ->
     let* (cont_label, brk_label, tmp_idx, insts, last_result) =
       codegen_expr acc rexpr
     in
     let new_inst = (NoValue, Store (t, last_result, Pointer t, LNamed name)) in
     Ok (cont_label, brk_label, tmp_idx, new_inst :: insts, last_result)
  | FunctionCall (rt, fexp, ats, args) ->
     let* acc = codegen_expr acc fexp in
     let (_, _, _, _, f_value) = acc in
     let+ (args_rev, acc) =
       Symtab.fold_left_bind
         (fun (args, acc) arg_expr ->
           let+ acc = codegen_expr acc arg_expr in
           let (_, _, _, _, arg_value) = acc in
           (arg_value :: args, acc))
         ([], acc) args
     in
     let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
     let res = match rt with
       | Void -> NoValue
       | _ -> LTemporary tmp_idx
     in
     let new_inst = (res, Call (rt, f_value,
                                List.combine ats (List.rev args_rev))) in
     (cont_label, brk_label, tmp_idx + 1, new_inst :: insts, res)
  | ItoF (at, exp, bt) ->
     let+ (cont_label, brk_label, tmp_idx, insts, v) =
       codegen_expr acc exp
     in
     let res = LTemporary tmp_idx in
     let new_inst = (res, ItoFP (at, v, bt)) in
     (cont_label, brk_label, tmp_idx + 1, new_inst :: insts, res)
  | FtoI (at, exp, bt) ->
     let+ (cont_label, brk_label, tmp_idx, insts, v) =
       codegen_expr acc exp
     in
     let res = LTemporary tmp_idx in
     let new_inst = (res, FPtoI (at, v, bt)) in
     (cont_label, brk_label, tmp_idx + 1, new_inst :: insts, res)
  | BitCast (at, exp, bt) ->
     let+ (cont_label, brk_label, tmp_idx, insts, v) =
       codegen_expr acc exp
     in
     let res = LTemporary tmp_idx in
     let new_inst = (res, BitCast (at, v, bt)) in
     (cont_label, brk_label, tmp_idx + 1, new_inst :: insts, res)

  | BinOp (t, op, l_expr, r_expr) ->
     let* acc = codegen_expr acc l_expr in
     let (_, _, _, _, l_value) = acc in
     let+ acc = codegen_expr acc r_expr in
     let (cont_label, brk_label, tmp_idx, insts, r_value) = acc in
     let res = LTemporary tmp_idx in

     let new_inst = match op with
       | Plus    -> Add (t, l_value, r_value)
       | Minus   -> Sub (t, l_value, r_value)
       | Times   -> Mul (t, l_value, r_value)
       | Divide  -> Div (t, l_value, r_value)
       | Modulus -> Rem (t, l_value, r_value)

       | Equal       -> CmpEq (t, l_value, r_value)
       | LessThan    -> CmpLt (t, l_value, r_value)
       | GreaterThan -> CmpGt (t, l_value, r_value)

       | And -> And (t, l_value, r_value)
       | Or  -> Or  (t, l_value, r_value)
       | Xor -> Xor (t, l_value, r_value)

       | LShift -> Shl (t, l_value, r_value)
       | RShift -> Shr (t, l_value, r_value)
     in

     (cont_label, brk_label, tmp_idx + 1, (res, new_inst) :: insts, res)
  | UnOp (t, op, expr) ->
     let+ acc = codegen_expr acc expr in
     let (cont_label, brk_label, tmp_idx, insts, result) = acc in
     let res = LTemporary tmp_idx in
     let new_inst = match op with
       | UMinus -> begin
           match t with
           | F64 -> FNeg (t, result)
           | _ -> Sub (t, LLiteral (Int 0), result)
         end
       | Not -> Xor (t, result, LLiteral (Int (-1)))
     in
     (cont_label, brk_label, tmp_idx + 1, (res, new_inst) :: insts, res)

let rec codegen_stmt acc stmt =
  let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
  match stmt with
  | Empty -> Ok acc
  | Decl (t, name, expr) ->
     let+ acc = codegen_expr acc expr in
     let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
     let res = LNamed name in
     let alloca_inst = (LNamed name, Alloca t) in
     let store_inst = (NoValue, Store (t, last_result, Pointer t, res)) in
     (cont_label, brk_label, tmp_idx,
      store_inst :: alloca_inst :: insts, NoValue)
  | Expr expr -> codegen_expr acc expr
  | Block (name, stmts) ->
     let new_label = (NoValue, Label name) in
     let block_entry_inst = (NoValue, Branch name) in
     let acc = (cont_label, brk_label, tmp_idx,
                new_label :: block_entry_inst :: insts, last_result) in
     let+ acc = Symtab.fold_left_bind codegen_stmt acc stmts in
     let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
     let end_label = name ^ "_end" in
     let branch_inst = (NoValue, Branch end_label) in
     let end_inst = (NoValue, Label end_label) in
     (cont_label, brk_label, tmp_idx,
      end_inst :: branch_inst :: insts, last_result)
  | IfElse (if_label, else_label, cond_expr, if_stmts, else_stmts) ->
     let* acc = codegen_expr acc cond_expr in
     let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
     let branch_inst =
       (NoValue,
        BranchCond (last_result, if_label, else_label))
     in
     let if_label_inst = (NoValue, Label if_label) in
     let ifelse_end_label = if_label ^ "_end" in
     let ifelse_end_label_inst = (NoValue, Label ifelse_end_label) in
     let if_end_branch_inst = (NoValue, Branch ifelse_end_label) in
     let else_label_inst = (NoValue, Label else_label) in
     let else_end_branch_inst = (NoValue, Branch ifelse_end_label) in
     let acc = (cont_label, brk_label, tmp_idx,
                if_label_inst :: branch_inst :: insts, NoValue) in
     let* acc = Symtab.fold_left_bind codegen_stmt acc if_stmts in
     let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
     let acc = (cont_label, brk_label, tmp_idx,
                else_label_inst :: if_end_branch_inst :: insts, NoValue) in
     let+ acc = Symtab.fold_left_bind codegen_stmt acc else_stmts in
     let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
     (cont_label, brk_label, tmp_idx,
      ifelse_end_label_inst :: else_end_branch_inst :: insts, NoValue)
  | While (while_label, cond_expr, while_stmts) ->
     let while_label_inst = (NoValue, Label while_label) in
     let while_end_label = while_label ^ "_end" in
     let while_end_label_inst = (NoValue, Label while_end_label) in
     let while_cond_label = while_label ^ "_cond" in
     let while_cond_label_inst = (NoValue, Label while_cond_label) in
     let branch_inst = (NoValue, Branch while_cond_label) in
     let acc = (Some while_cond_label, Some while_end_label, tmp_idx,
                while_label_inst :: branch_inst :: insts, NoValue) in
     let* acc = Symtab.fold_left_bind codegen_stmt acc while_stmts in
     let (cont_label, brk_label, tmp_idx, insts, _) = acc in
     let branch_into_cond_inst = (NoValue, Branch while_cond_label) in
     let acc =
       (cont_label, brk_label, tmp_idx,
        while_cond_label_inst :: branch_into_cond_inst :: insts, NoValue)
     in
     let+ acc = codegen_expr acc cond_expr in
     let (_, _, tmp_idx, insts, last_result) = acc in
     let branch_cond_inst =
       (NoValue,
        BranchCond (last_result, while_label, while_end_label))
     in
     (None, None, tmp_idx,
      while_end_label_inst :: branch_cond_inst :: insts, NoValue)
  | For (for_label, decl, cond_expr, inc_expr, for_stmts) ->
     let* acc = codegen_stmt acc (Decl decl) in
     let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
     let for_label_inst = (NoValue, Label for_label) in
     let for_end_label = for_label ^ "_end" in
     let for_end_label_inst = (NoValue, Label for_end_label) in
     let for_body_label = for_label ^ "_body" in
     let for_body_label_inst = (NoValue, Label for_body_label) in
     let for_inc_label = for_label ^ "_inc" in
     let for_inc_label_inst = (NoValue, Label for_inc_label) in
     let branch_into_for_inst = (NoValue, Branch for_label) in
     let acc =
       (Some for_inc_label, Some for_end_label, tmp_idx,
        for_label_inst :: branch_into_for_inst :: insts, NoValue)
     in
     let* acc = codegen_expr acc cond_expr in
     let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
     let branch_cond_inst =
       (NoValue,
        BranchCond (last_result, for_body_label, for_end_label)) in
     let acc = (cont_label, brk_label, tmp_idx,
                for_body_label_inst :: branch_cond_inst :: insts, NoValue) in
     let* acc = Symtab.fold_left_bind codegen_stmt acc for_stmts in
     let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
     let branch_into_inc_inst = (NoValue, Branch for_inc_label) in
     let acc =
       (cont_label, brk_label, tmp_idx,
        for_inc_label_inst :: branch_into_inc_inst :: insts,
        NoValue)
     in
     let+ acc = codegen_expr acc inc_expr in
     let (_, _, tmp_idx, insts, _) = acc in
     let branch_back_cond_inst = (NoValue, Branch for_label) in
     (None, None, tmp_idx,
      for_end_label_inst :: branch_back_cond_inst :: insts,
      NoValue)
  | Continue ->
     begin
       match cont_label with
       | None -> Error "Error: Invalid 'continue'"
       | Some label ->
          let branch_inst = (NoValue, Branch label) in
          Ok (cont_label, brk_label, tmp_idx, branch_inst :: insts, NoValue)
     end
  | Break ->
     begin
       match brk_label with
       | None -> Error "Error: Invalid 'break'"
       | Some label ->
          let branch_inst = (NoValue, Branch label) in
          Ok (cont_label, brk_label, tmp_idx, branch_inst :: insts, NoValue)
     end
  | Return (exp_opt) ->
     begin
       match exp_opt with
       | None ->
          let ret_inst = (NoValue, Ret (Void, NoValue)) in
          Ok (cont_label, brk_label, tmp_idx, ret_inst :: insts, NoValue)
       | Some exp ->
          let+ acc = codegen_expr acc exp in
          let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
          let ret_inst = (NoValue, Ret (get_ir_expr_type exp, last_result)) in
          (cont_label, brk_label, tmp_idx, ret_inst :: insts, NoValue)
     end

let rec serialize_type t = match t with
  | Pointer t -> (serialize_type t) ^ "*"
  | Void -> "void"

  (* The Function type is actually a function pointer. *)
  | Function (ats, rt) ->
     (serialize_type rt) ^ "(" ^
       (String.concat ", " (List.map serialize_type ats)) ^
         ")*"

  | I1  | U1  -> "i1"
  | I8  | U8  -> "i8"
  | I16 | U16 -> "i16"
  | I32 | U32 -> "i32"
  | I64 | U64 -> "i64"
  | F32 -> "float"
  | F64 -> "double"


let serialize_literal l = match l with
  | Int i -> string_of_int i
  | Float f -> string_of_float f

let serialize_value v = match v with
  | NoValue -> ""
  | LLiteral l -> serialize_literal l
  | LNamed name -> name
  | LTemporary i -> "%__tmp." ^ (string_of_int i)

let is_unsigned t = match t with
  | U1 | U8 | U16 | U32 | U64 -> true
  | _ -> false

let is_float t = match t with
  | F64 | F32 -> true
  | _ -> false

let serialize_irt irt_roots =
  let rec serialize_inst = fun (value, inst) ->
    let inst_str i = match inst with
      | NoInst -> ""
      | Alloca t -> "alloca " ^ (serialize_type t)
      | Load (lt, t, v) -> "load " ^ (serialize_type lt) ^ ", "
                           ^ (serialize_type t) ^ " " ^ (serialize_value v)
      | Store (t1, v1, t2, v2) -> String.concat " " ["store";
                                                     serialize_type t1;
                                                     (serialize_value v1) ^ ",";
                                                     serialize_type t2;
                                                     serialize_value v2]
      | Label s -> s ^ ":"
      | Branch l -> "br label %" ^ l
      | BranchCond (v, ifl, elsel) -> "br i1 " ^ (serialize_value v) ^ ", label %"
                                      ^ ifl ^ ", label %" ^ elsel
      | Ret (t, v) -> String.concat " " ["ret"; serialize_type t; serialize_value v]
      | Call (t, v, args) ->
         let args_s = String.concat ", "
                        (List.map
                           (fun (t, v) -> (serialize_type t)
                                          ^ " " ^ (serialize_value v))
                           args) in
         String.concat " " ["call"; serialize_type t; serialize_value v;
                            "(" ^ args_s ^ ")"]

      | Add (t, v1, v2) -> String.concat " "
                             [if is_float t then "fadd" else "add";
                              serialize_type t;
                              (serialize_value v1) ^ ",";
                              serialize_value v2]
      | Sub (t, v1, v2) -> String.concat " "
                             [if is_float t then "fsub" else "sub";
                              serialize_type t;
                              (serialize_value v1) ^ ",";
                              serialize_value v2]
      | Mul (t, v1, v2) -> String.concat " "
                             [if is_float t then "fmul" else "mul";
                              serialize_type t;
                              (serialize_value v1) ^ ",";
                              serialize_value v2]
      | Div (t, v1, v2) -> String.concat " "
                             [if is_unsigned t then "udiv" else
                                if is_float t then "fdiv" else "sdiv";
                              serialize_type t;
                              (serialize_value v1) ^ ",";
                              serialize_value v2]
      | Rem (t, v1, v2) -> String.concat " "
                             [if is_unsigned t then "urem" else
                                if is_float t then "frem" else "srem";
                              serialize_type t;
                              (serialize_value v1) ^ ",";
                              serialize_value v2]
      | FNeg (t, v) -> String.concat " "
                         ["fneg"; serialize_type t; serialize_value v]

      | CmpEq (t, v1, v2) -> String.concat " "
                               [if is_float t then "fcmp" else "icmp";
                                if is_float t then "oeq" else "eq";
                                serialize_type t;
                                (serialize_value v1) ^ ",";
                                serialize_value v2]
      | CmpLt (t, v1, v2) -> String.concat " "
                               [if is_float t then "fcmp" else "icmp";
                                if is_unsigned t then "ult" else
                                  if is_float t then "olt" else "slt";
                                serialize_type t;
                                (serialize_value v1) ^ ",";
                                serialize_value v2]
      | CmpGt (t, v1, v2) -> String.concat " "
                               [if is_float t then "fcmp" else "icmp";
                                if is_unsigned t then "ugt" else
                                  if is_float t then "ogt" else "sgt";
                                serialize_type t;
                                (serialize_value v1) ^ ",";
                                serialize_value v2]

      | And (t, v1, v2) -> String.concat " " ["and"; serialize_type t;
                                              (serialize_value v1) ^ ",";
                                              serialize_value v2]
      | Or (t, v1, v2) -> String.concat " " ["or"; serialize_type t;
                                             (serialize_value v1) ^ ",";
                                             serialize_value v2]
      | Xor (t, v1, v2) -> String.concat " " ["xor"; serialize_type t;
                                              (serialize_value v1) ^ ",";
                                              serialize_value v2]

      | Shl (t, v1, v2) -> String.concat " " ["shl"; serialize_type t;
                                              (serialize_value v1) ^ ",";
                                              serialize_value v2]
      | Shr (t, v1, v2) -> String.concat " "
                             [if is_unsigned t then "lshr" else "ashr";
                              serialize_type t;
                              (serialize_value v1) ^ ",";
                              serialize_value v2]

      | ItoFP (at, v, bt) ->
         String.concat " "
           [if is_unsigned at then "uitofp" else "sitofp";
            serialize_type at;
            serialize_value v;
            "to";
            serialize_type bt]
      | FPtoI (at, v, bt) ->
         String.concat " "
           [if is_unsigned bt then "fptoui" else "fptosi";
            serialize_type at;
            serialize_value v;
            "to";
            serialize_type bt]
      | BitCast (at, v, bt) ->
         String.concat " "
           ["bitcast";
            serialize_type at;
            serialize_value v;
            "to";
            serialize_type bt]
    in
    match value with
    | NoValue -> inst_str inst
    | v -> (serialize_value v) ^ " = " ^ (inst_str inst)
  in

  let serialize_irt str_insts root =
    match root with
    | StaticDecl (t, name, l) ->
       let t_str = serialize_type t in
       let l_str = serialize_literal l in
       let s = String.concat " " [name; "="; "global"; t_str; l_str] in
       Ok (s :: str_insts)
    | FuncDecl (rt, funcname, args, body) ->
       let t_str = serialize_type rt in
       let args_str = String.concat ", "
                        (List.map
                           (fun (t, n) -> (serialize_type t) ^ " " ^ n)
                           args) in
       let ns = "define " ^ t_str ^ " " ^ funcname ^ "(" ^ args_str ^ ") {" in
       let+ (_, _, _, insts, _) =
         Symtab.fold_left_bind
           codegen_stmt (None, None, 0, [], NoValue) (List.rev body)
       in
       "}" :: ((List.map serialize_inst insts) @ (ns :: str_insts))
    | FuncFwdDecl (rt, funcname, args, true) ->
       let t_str = serialize_type rt in
       let args_str = String.concat ", "
                        (List.map
                           (fun (t, n) -> (serialize_type t) ^ " " ^ n)
                           args) in
       let ns = "declare " ^ t_str ^ " " ^ funcname ^ "(" ^ args_str ^ ");" in
       Ok (ns :: str_insts)
    | FuncFwdDecl (_, _, _, false) -> Ok str_insts
  in
  let+ l = Symtab.fold_left_bind serialize_irt [] irt_roots in
  String.concat "\n" (List.rev l)
