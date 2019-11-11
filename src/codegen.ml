
(*
 * LLVM Code generation.
 *)

module ScopeM = Map.Make(String)

type llvm_type = I1 | I8 | I16 | I32 | I64
                 | U1 | U8 | U16 | U32 | U64
                 | Pointer of llvm_type
                 | Function of llvm_type list * llvm_type
                 | Void

type ir_literal = Int of int
type ir_op = Plus | LessThan
type ir_expr = Identifier of llvm_type * string
             | Literal of llvm_type * ir_literal
             | Assignment of llvm_type * string * ir_expr
             | FunctionCall of llvm_type * ir_expr * llvm_type list * ir_expr list
             | BinOp of llvm_type * ir_op * ir_expr * ir_expr
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

type llvm_value = LTemporary of int
                | LNamed of string
                | LLiteral of ir_literal
                | NoValue
type llvm_inst = Alloca of llvm_type
               | Load of llvm_type * llvm_type * llvm_value
               | Store of llvm_type * llvm_value * llvm_type * llvm_value
               | Call of llvm_type * llvm_value * llvm_type list * llvm_value list
               | Ret of llvm_type * llvm_value
               | Add of llvm_type * llvm_value * llvm_value
               | ICmpSlt of llvm_type * llvm_value * llvm_value
               | Label of string
               | BranchCond of llvm_value * string * string
               | Branch of string
               | NoInst


let rec llvm_type_of_silktype t =
  match t with
  | Symtab.Function (argtypes, rettype) ->
     let llargtypes = List.map llvm_type_of_silktype argtypes in
     let llrettype = llvm_type_of_silktype rettype in
     Function (llargtypes, llrettype)
  | Symtab.Bool -> I1
  | Symtab.Int -> I32
  | Symtab.Void -> Void
  | Symtab.NewType (_, t) -> llvm_type_of_silktype t

(* TODO binops, other things *)
let resolve_literal l = match l with
  | Parsetree.Literal l -> begin
      match l with
      | Parsetree.LInt i -> Ok (Int i)
    end
  | _ -> Error "Error: Could not resolve static value at compile time"

let get_ir_expr_type ir_exp = match ir_exp with
  | Identifier (t, _) -> t
  | Literal (t, _) -> t
  | Assignment (t, _, _) -> t
  | FunctionCall (t, _, _, _) -> t
  | BinOp (t, _, _, _) -> t

let construct_ir_tree ast symtab =
  let rec find_in_scope scope_stack symtab_stack name =
    match symtab_stack with
    | [] -> ([], [], Symtab.SymTab.find name symtab)
    | z :: zs ->
       match Symtab.SymTab.find_opt name z with
       | Some v -> (scope_stack, symtab_stack, v)
       | None -> find_in_scope (List.tl scope_stack) zs name
  in

  let rec find_symtab_stack name symtab_stack =
    match symtab_stack with
    | [] -> Symtab.SymTab.find_opt name symtab
    | (z :: zs) ->
       match Symtab.SymTab.find_opt name z with
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
    | Parsetree.Literal (Parsetree.LInt i) ->
       Ok (Literal (I32, Int i))
    | Parsetree.Assignment (name, exp) ->
       begin
         let symbol = find_symtab_stack name symtab_stack in
         match symbol with
         | Some (Symtab.Type _) -> Error ("Error: Expected value, found type: " ^ name)
         | Some (Symtab.Value (_, type_, _)) ->
            let t = llvm_type_of_silktype type_ in
            Result.map
              (fun ir_exp -> Assignment (t, ScopeM.find name scope_map, ir_exp))
              (map_expr scope_map symtab_stack exp)
         | None -> Error ("Error: Identifier " ^ name ^ " undefined")
       end
    | Parsetree.FunctionCall (fexp, argexps) ->
       Result.bind (map_expr scope_map symtab_stack fexp)
         begin
           fun ir_fexp ->
           match (get_ir_expr_type ir_fexp) with
           | Function (argtypes, rettype) ->
              let add_arg args ar_exp =
                Result.map (fun ir_argexp -> ir_argexp :: args)
                  (map_expr scope_map symtab_stack ar_exp)
              in
              Result.map
                (fun args -> FunctionCall (rettype, ir_fexp, argtypes, args))
                (Symtab.fold_left_bind add_arg [] argexps)
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
            | Parsetree.LessThan -> Ok (BinOp (get_ir_expr_type l, LessThan, l, r))
       end
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
      match Symtab.SymTab.find (string_of_int block_idx) (List.hd symtab_stack) with
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
         Result.map
           (fun ir_exp -> (t, name, resolved_name, ir_exp))
           (map_expr scope_map symtab_stack expr)
    in

    let (block_idx, scope_stack, scope_map, symtab_stack, ir_stmts) = acc in
    match stmt with
    | Parsetree.Empty -> Ok acc
    | Parsetree.Decl vd ->
       Result.map
         (fun (t, name, resolved_name, exp) ->
           let new_stmt = Decl (t, resolved_name, exp) in
           (block_idx, scope_stack, (ScopeM.add name resolved_name scope_map),
            symtab_stack, new_stmt :: ir_stmts))
         (map_decl vd acc)
    | Parsetree.Expr expr ->
       Result.map
         (fun ir_exp ->
           (block_idx, scope_stack, scope_map,
            symtab_stack, (Expr ir_exp) :: ir_stmts))
         (map_expr scope_map symtab_stack expr)
    | Parsetree.Block blk -> Result.map
                               (fun stmts ->
                                 (block_idx + 1, scope_stack, scope_map, symtab_stack,
                                  Block (blk_name block_idx scope_stack, stmts)
                                  :: ir_stmts))
                               (map_blk blk acc)
    | Parsetree.IfElse (cond_expr, ifblock, elseblock) ->
       begin
         match (ifblock, elseblock) with
         | (Parsetree.Block ifstmts, Parsetree.Block elsestmts) ->
            Result.bind (map_expr scope_map symtab_stack cond_expr)
              (fun cond_ir_exp ->
                Result.bind (map_blk ifstmts acc)
                  (fun if_ir_stmts ->
                    let acc = (block_idx + 1, scope_stack,
                               scope_map, symtab_stack, ir_stmts) in
                    Result.map
                      (fun else_ir_stmts ->
                        let new_stmt = IfElse (blk_name block_idx scope_stack,
                                               blk_name (block_idx + 1) scope_stack,
                                               cond_ir_exp, if_ir_stmts,
                                               else_ir_stmts) in
                        (block_idx + 2, scope_stack, scope_map, symtab_stack,
                         new_stmt :: ir_stmts))
                      (map_blk elsestmts acc)))
         | _ -> Error "Error: Not a block"
       end
    | Parsetree.While (cond_expr, whileblock) ->
       begin
         match whileblock with
         | Parsetree.Block whilestmts ->
            Result.bind (map_expr scope_map symtab_stack cond_expr)
              (fun cond_ir_exp ->
                Result.map
                  (fun while_ir_stmts ->
                    let new_stmt = While (blk_name block_idx scope_stack,
                                          cond_ir_exp, while_ir_stmts) in
                    (block_idx + 1, scope_stack, scope_map,
                     symtab_stack, new_stmt :: ir_stmts))
                  (map_blk whilestmts acc))
         | _ -> Error "Error: Not a block"
       end
    | Parsetree.For (vd, cond_expr, inc_expr, forblock) ->
       begin
         match forblock with
         | Parsetree.Block forstmts ->
            let new_symtab = match Symtab.SymTab.find
                                     (string_of_int block_idx)
                                     (List.hd symtab_stack) with
              | Symtab.Value (_, _, Some new_symtab) -> new_symtab
              | _ -> Symtab.SymTab.empty
            in
            let new_symtab_stack = new_symtab :: symtab_stack in
            let new_scope_stack = (string_of_int block_idx) :: scope_stack in
            let new_acc = (block_idx, new_scope_stack, scope_map, new_symtab_stack,
                           ir_stmts) in
            Result.bind (map_decl vd new_acc)
              (fun (t, name, resolved_name, exp) ->
                let (block_idx, scope_stack, scope_map,
                     symtab_stack, ir_stmts) = acc in
                let scope_map = ScopeM.add name resolved_name scope_map in
                let acc = (block_idx, scope_stack, scope_map,
                           symtab_stack, ir_stmts) in
                Result.bind (map_expr scope_map new_symtab_stack cond_expr)
                  (fun cond_ir_exp ->
                    Result.bind (map_expr scope_map new_symtab_stack inc_expr)
                      (fun inc_ir_exp ->
                        Result.map
                          (fun for_ir_stmts ->
                            let new_stmt = For (blk_name block_idx scope_stack,
                                                (t, resolved_name, exp),
                                                cond_ir_exp, inc_ir_exp,
                                                for_ir_stmts) in
                            (block_idx + 1, scope_stack, scope_map, symtab_stack,
                             new_stmt :: ir_stmts))
                          (map_blk forstmts acc))))
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
       Result.map
         (fun new_stmt ->
           (block_idx, scope_stack, scope_map, symtab_stack, new_stmt :: ir_stmts))
         new_stmt_res

  and map_stmts stmts scope_stack scope_map symtab_stack =
    Result.map (fun (_, _, _, _, ir_stmts) -> ir_stmts)
      (Symtab.fold_left_bind map_stmt (0, scope_stack, scope_map,
                                       symtab_stack, []) stmts)
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
         match (Symtab.SymTab.find name symtab) with
         | Symtab.Type _ -> Error ("Error: Expected value, found type: " ^ name)
         | Symtab.Value (_, type_, _) ->
            let t = llvm_type_of_silktype type_ in
            let resolved_name = "@" ^ name in
            Result.map
              (fun l ->
                (ScopeM.add name resolved_name scope_map,
                 (StaticDecl (t, resolved_name, l)) :: decls))
              (resolve_literal expr)
       end
    | Parsetree.FuncDecl fd ->
       begin
         let (name, args_, _, body) = fd in
         let value_ = Symtab.SymTab.find name symtab in
         match value_ with
         | Symtab.Type _ -> Error ("Error: Expected value, found type: " ^ name)
         | Symtab.Value (_, type_, inner_st) ->
            match type_ with
            | Symtab.Function (argtypes, rettype) ->
               let rt = llvm_type_of_silktype rettype in
               let args = List.map
                            (fun ((n, _), t) -> (llvm_type_of_silktype t, "%" ^ n))
                            (List.combine args_ argtypes)
               in
               begin
                 match body with
                 | Parsetree.Block stmts ->
                    begin
                      let resolved_name = "@" ^ name in
                      let scope_map = ScopeM.add name resolved_name scope_map in
                      Result.map
                        (fun ir_stmts ->
                          (scope_map,
                           (FuncDecl (rt, resolved_name, args, ir_stmts)) :: decls))
                        (map_stmts stmts [name] scope_map [Option.get inner_st])
                    end
                 | _ -> Error "Error: Function body is not a block"
               end
            | _ -> Error ("Error: Symbol " ^ name ^ " is not a function")
       end
  in
  Result.map
    (fun (_, l) -> List.rev l)
    (Symtab.fold_left_bind map_top_decl (ScopeM.empty, []) ast)

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
  | Literal (t, lit) ->
     Ok (cont_label, brk_label, tmp_idx, insts, LLiteral lit)
  | Assignment (t, name, rexpr) ->
     Result.bind (codegen_expr acc rexpr)
       (fun acc ->
         let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
         let new_inst = (NoValue, Store (t, last_result, Pointer t, LNamed name)) in
         Ok (cont_label, brk_label, tmp_idx, new_inst :: insts, last_result))
  | FunctionCall (rt, fexp, ats, args) ->
     Result.bind (codegen_expr acc fexp)
       begin
         fun acc ->
         let (_, _, _, _, f_value) = acc in
         let args_resolved =
           Symtab.fold_left_bind
             (fun (args, acc) arg_expr ->
               Result.map (fun acc ->
                   let (_, _, _, _, arg_value) = acc in (arg_value :: args, acc))
                 (codegen_expr acc arg_expr))
             ([], acc) args in
         Result.map
           (fun (args_rev, acc) ->
             let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
             let res = LTemporary tmp_idx in
             let new_inst = (res, Call (rt, f_value, ats, List.rev args_rev)) in
             (cont_label, brk_label, tmp_idx + 1, new_inst :: insts, res)
           ) args_resolved
       end
  | BinOp (t, op, l_expr, r_expr) ->
     Result.bind (codegen_expr acc l_expr)
       begin
         fun acc ->
         let (_, _, _, _, l_value) = acc in
         Result.map
           begin
             fun acc ->
             let (cont_label, brk_label, tmp_idx, insts, r_value) = acc in
             match op with
             | Plus ->
                let res = LTemporary tmp_idx in
                let new_inst = (res, Add (t, l_value, r_value)) in
                (cont_label, brk_label, tmp_idx + 1, new_inst :: insts, res)
             | LessThan ->
                let res = LTemporary tmp_idx in
                let new_inst = (res, ICmpSlt (t, l_value, r_value)) in
                (cont_label, brk_label, tmp_idx + 1, new_inst :: insts, res)
           end
           (codegen_expr acc r_expr)
       end

let rec codegen_stmt acc stmt =
  let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
  match stmt with
  | Empty -> Ok acc
  | Decl (t, name, expr) ->
     Result.bind (codegen_expr acc expr)
       (fun acc ->
         let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
         let res = LNamed name in
         let alloca_inst = (LNamed name, Alloca t) in
         let store_inst = (NoValue, Store (t, last_result, Pointer t, res)) in
         Ok (cont_label, brk_label, tmp_idx,
             store_inst :: alloca_inst :: insts, NoValue))
  | Expr expr -> codegen_expr acc expr
  | Block (name, stmts) ->
     let new_label = (NoValue, Label name) in
     let acc = (cont_label, brk_label, tmp_idx, new_label :: insts, last_result) in
     let acc_r = Symtab.fold_left_bind codegen_stmt acc stmts in
     Result.map
       (fun acc ->
         let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
         let end_label = name ^ "_end" in
         let branch_inst = (NoValue, Branch end_label) in
         let end_inst = (NoValue, Label end_label) in
         (cont_label, brk_label, tmp_idx,
          end_inst :: branch_inst :: insts, last_result))
       acc_r
  | IfElse (if_label, else_label, cond_expr, if_stmts, else_stmts) ->
     Result.bind (codegen_expr acc cond_expr)
       (fun acc ->
         let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
         let branch_inst = (NoValue,
                            BranchCond (last_result, if_label, else_label)) in
         let if_label_inst = (NoValue, Label if_label) in
         let ifelse_end_label = if_label ^ "_end" in
         let ifelse_end_label_inst = (NoValue, Label ifelse_end_label) in
         let if_end_branch_inst = (NoValue, Branch ifelse_end_label) in
         let else_label_inst = (NoValue, Label else_label) in
         let else_end_branch_inst = (NoValue, Branch ifelse_end_label) in
         let acc = (cont_label, brk_label, tmp_idx,
                    if_label_inst :: branch_inst :: insts, NoValue) in
         Result.bind (Symtab.fold_left_bind codegen_stmt acc if_stmts)
           (fun acc ->
             let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
             let acc = (cont_label, brk_label, tmp_idx,
                        else_label_inst :: if_end_branch_inst :: insts, NoValue) in
             Result.map
               (fun acc ->
                 let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
                 (cont_label, brk_label, tmp_idx,
                  ifelse_end_label_inst :: else_end_branch_inst :: insts, NoValue))
               (Symtab.fold_left_bind codegen_stmt acc else_stmts)))
  | While (while_label, cond_expr, while_stmts) ->
     let while_label_inst = (NoValue, Label while_label) in
     let while_end_label = while_label ^ "_end" in
     let while_end_label_inst = (NoValue, Label while_end_label) in
     let while_cond_label = while_label ^ "_cond" in
     let while_cond_label_inst = (NoValue, Label while_cond_label) in
     let branch_inst = (NoValue, Branch while_cond_label) in
     let acc = (Some while_cond_label, Some while_end_label, tmp_idx,
                while_label_inst :: branch_inst :: insts, NoValue) in
     Result.bind (Symtab.fold_left_bind codegen_stmt acc while_stmts)
       (fun acc ->
         let (cont_label, brk_label, tmp_idx, insts, _) = acc in
         let branch_into_cond_inst = (NoValue, Branch while_cond_label) in
         let acc = (cont_label, brk_label, tmp_idx,
                    while_cond_label_inst :: branch_into_cond_inst :: insts, NoValue)
         in
         Result.map
           (fun acc ->
             let (_, _, tmp_idx, insts, last_result) = acc in
             let branch_cond_inst =
               (NoValue,
                BranchCond (last_result, while_label, while_end_label)) in
             (None, None, tmp_idx,
              while_end_label_inst :: branch_cond_inst :: insts, NoValue))
           (codegen_expr acc cond_expr))
  | For (for_label, decl, cond_expr, inc_expr, for_stmts) ->
     Result.bind (codegen_stmt acc (Decl decl))
       (fun acc ->
         let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
         let for_label_inst = (NoValue, Label for_label) in
         let for_end_label = for_label ^ "_end" in
         let for_end_label_inst = (NoValue, Label for_end_label) in
         let for_body_label = for_label ^ "_body" in
         let for_body_label_inst = (NoValue, Label for_body_label) in
         let for_inc_label = for_label ^ "_inc" in
         let for_inc_label_inst = (NoValue, Label for_inc_label) in
         let branch_into_for_inst = (NoValue, Branch for_label) in
         let acc = (Some for_inc_label, Some for_end_label, tmp_idx,
                    for_label_inst :: branch_into_for_inst :: insts, NoValue) in
         Result.bind (codegen_expr acc cond_expr)
           (fun acc ->
             let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
             let branch_cond_inst =
               (NoValue,
                BranchCond (last_result, for_body_label, for_end_label)) in
             let acc = (cont_label, brk_label, tmp_idx,
                        for_body_label_inst :: branch_cond_inst :: insts, NoValue) in
             Result.bind (Symtab.fold_left_bind codegen_stmt acc for_stmts)
               (fun acc ->
                 let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
                 let branch_into_inc_inst = (NoValue, Branch for_inc_label) in
                 let acc = (cont_label, brk_label, tmp_idx,
                            for_inc_label_inst :: branch_into_inc_inst :: insts,
                            NoValue) in
                 Result.map
                   (fun acc ->
                     let (_, _, tmp_idx, insts, _) = acc in
                     let branch_back_cond_inst = (NoValue, Branch for_label) in
                     (None, None, tmp_idx,
                      for_end_label_inst :: branch_back_cond_inst :: insts,
                      NoValue))
                   (codegen_expr acc inc_expr))))
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
          Result.map
            (fun acc ->
              let (cont_label, brk_label, tmp_idx, insts, last_result) = acc in
              let ret_inst = (NoValue, Ret (get_ir_expr_type exp, last_result)) in
              (cont_label, brk_label, tmp_idx, ret_inst :: insts, NoValue))
            (codegen_expr acc exp)
     end

let rec serialize_type t = match t with
  | Pointer t -> (serialize_type t) ^ "*"
  | I32 -> "i32"
  | I1 -> "i1"
  | Void -> "void"
  (*TODO*)
  | _ -> "<type>"

let serialize_literal l = match l with
  | Int i -> string_of_int i

let serialize_value v = match v with
  | NoValue -> ""
  | LLiteral (Int i) -> string_of_int i
  | LNamed name -> name
  | LTemporary i -> "%__tmp." ^ (string_of_int i)


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
      | Add (t, v1, v2) -> String.concat " " ["add"; serialize_type t;
                                              (serialize_value v1) ^ ",";
                                              serialize_value v2]
      | ICmpSlt (t, v1, v2) -> String.concat " " ["icmp slt"; serialize_type t;
                                                  (serialize_value v1) ^ ",";
                                                  serialize_value v2]
      | Label s -> s ^ ":"
      | Branch l -> "br label %" ^ l
      | BranchCond (v, ifl, elsel) -> "br i1 " ^ (serialize_value v) ^ ", label %"
                                      ^ ifl ^ ", label %" ^ elsel
      | Ret (t, v) -> String.concat " " ["ret"; serialize_type t; serialize_value v]
      | Call (t, v, ts, vs) ->
         let ts_s = String.concat ", " (List.map serialize_type ts) in
         let vs_s = String.concat ", " (List.map serialize_value vs) in
         String.concat " " ["call"; serialize_type t; serialize_value v;
                            "(" ^ ts_s ^ ")"; "(" ^ vs_s ^ ")"]
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
       s :: str_insts
    | FuncDecl (rt, funcname, args, body) ->
       let t_str = serialize_type rt in
       let args_str = String.concat ", "
                        (List.map
                           (fun (t, n) ->
                             (serialize_type t) ^ " " ^ n)
                           args) in
       let ns = "define " ^ t_str ^ " " ^ funcname ^ "(" ^ args_str ^ ") {" in
       let bstmts =
         Symtab.fold_left_bind
           codegen_stmt (None, None, 0, [], NoValue) (List.rev body) in
       let bstr = match bstmts with
         | Ok (_, _, _, insts, _) ->
            (List.map serialize_inst (List.rev insts)) @ str_insts
         | Error e -> e :: str_insts
       in
       ns :: (String.concat "\n" bstr) :: "}" :: str_insts
  in
  String.concat "\n" (List.fold_left serialize_irt [] (List.rev irt_roots))

