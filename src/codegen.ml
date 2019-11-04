
(*
 * LLVM Code generation.
 *)

type block_type = Block | IfElse | For | While

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

type llvm_value = LTemporary of llvm_type * int
                | LNamed of llvm_type * string
                | LLiteral of ir_literal
                | NoValue
type llvm_inst = Alloca of llvm_type
               | Load of llvm_value
               | Store of llvm_value * llvm_value
               | Call of llvm_type * llvm_value * llvm_value list
               | Ret of llvm_value
               | Add of llvm_value * llvm_value
               | Label of string * llvm_inst
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

  let rec map_expr scope_stack symtab_stack expr =
    match expr with
    | Parsetree.Identifier name ->
       begin
         let (scopes, symtabs, symbol) =
           find_in_scope scope_stack symtab_stack name
         in
         match symbol with
         | Symtab.Type _ -> Error ("Error: Expected value, found type: " ^ name)
         | Symtab.Value (_, type_, _) ->
            let t = llvm_type_of_silktype type_ in
            let prefix = match scopes with
              | [] -> "@"
              | z :: zs -> "%" ^ (String.concat "." (List.rev scopes))
            in
            Ok (Identifier (t, prefix ^ name))
       end
    | Parsetree.Literal (Parsetree.LInt i) ->
       Ok (Literal (I32, Int i))
    | Parsetree.Assignment (name, exp) ->
       begin
         let (scopes, symtabs, symbol) =
           find_in_scope scope_stack symtab_stack name
         in
         match symbol with
         | Symtab.Type _ -> Error ("Error: Expected value, found type: " ^ name)
         | Symtab.Value (_, type_, _) ->
            let t = llvm_type_of_silktype type_ in
            let prefix = match scopes with
              | [] -> "@"
              | z :: zs -> "%" ^ (String.concat "." (List.rev scopes))
            in
            Result.map
              (fun ir_exp -> Assignment (t, prefix ^ name, ir_exp))
              (map_expr scope_stack symtab_stack exp)
       end
    | Parsetree.FunctionCall (fexp, argexps) ->
       Result.bind (map_expr scope_stack symtab_stack fexp)
         begin
           fun ir_fexp ->
           match (get_ir_expr_type ir_fexp) with
           | Function (argtypes, rettype) ->
              let add_arg args ar_exp =
                Result.map (fun ir_argexp -> ir_argexp :: args)
                  (map_expr scope_stack symtab_stack ar_exp)
              in
              Result.map
                (fun args -> FunctionCall (rettype, ir_fexp, argtypes, args))
                (Symtab.fold_left_bind add_arg [] argexps)
           | _ -> Error "Error: Not a function"
         end
    | Parsetree.BinOp (lexp_, op_, rexp_) ->
       begin
         let lexp = map_expr scope_stack symtab_stack lexp_ in
         let rexp = map_expr scope_stack symtab_stack rexp_ in
         match (lexp, rexp) with
         | (Error e, _) -> Error e
         | (_, Error e) -> Error e
         | (Ok l, Ok r) ->
            match op_ with
            | Parsetree.Plus -> Ok (BinOp (get_ir_expr_type l, Plus, l, r))
            | Parsetree.LessThan -> Ok (BinOp (I1, LessThan, l, r))
       end
    | Parsetree.Index (array_exp, _) -> map_expr scope_stack symtab_stack
                                          array_exp
  in

  let rec map_stmt acc stmt =
    let map_blk blk acc =
      let (block_idx, scope_stack, symtab_stack, ir_stmts) = acc in
      match Symtab.SymTab.find (string_of_int block_idx) (List.hd symtab_stack) with
      | Symtab.Value (_, _, Some new_symtab) ->
         (map_stmts
            blk
            ((string_of_int block_idx) :: scope_stack)
            (new_symtab :: symtab_stack))
      | _ -> Error "Error: Not a block"
    in

    let map_decl vd acc =
      let (block_idx, scope_stack, symtab_stack, ir_stmts) = acc in
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
         Result.map
           (fun ir_exp -> (t, "%" ^ prefix ^ "." ^ name, ir_exp))
           (map_expr scope_stack symtab_stack expr)
    in

    let (block_idx, scope_stack, symtab_stack, ir_stmts) = acc in
    match stmt with
    | Parsetree.Empty -> Ok acc
    | Parsetree.Decl vd ->
       Result.map
         (fun ir_dec ->
           let new_stmt = Decl ir_dec in
           (block_idx, scope_stack, symtab_stack, new_stmt :: ir_stmts))
         (map_decl vd acc)
    | Parsetree.Expr expr ->
       Result.map
         (fun ir_exp ->
           (block_idx, scope_stack, symtab_stack, (Expr ir_exp) :: ir_stmts))
         (map_expr scope_stack symtab_stack expr)
    | Parsetree.Block blk -> Result.map
                               (fun stmts ->
                                 (block_idx + 1, scope_stack, symtab_stack,
                                  Block (string_of_int block_idx, stmts) :: ir_stmts))
                               (map_blk blk acc)
    | Parsetree.IfElse (cond_expr, ifblock, elseblock) ->
       begin
         match (ifblock, elseblock) with
         | (Parsetree.Block ifstmts, Parsetree.Block elsestmts) ->
            Result.bind (map_expr scope_stack symtab_stack cond_expr)
              (fun cond_ir_exp ->
                Result.bind (map_blk ifstmts acc)
                  (fun if_ir_stmts ->
                    let acc = (block_idx + 1, scope_stack, symtab_stack, ir_stmts) in
                    Result.map
                      (fun else_ir_stmts ->
                        let new_stmt = IfElse (string_of_int block_idx,
                                               string_of_int (block_idx + 1),
                                               cond_ir_exp, if_ir_stmts,
                                               else_ir_stmts) in
                        (block_idx + 2, scope_stack, symtab_stack,
                         new_stmt :: ir_stmts))
                      (map_blk elsestmts acc)))
         | _ -> Error "Error: Not a block"
       end
    | Parsetree.While (cond_expr, whileblock) ->
       begin
         match whileblock with
         | Parsetree.Block whilestmts ->
            Result.bind (map_expr scope_stack symtab_stack cond_expr)
              (fun cond_ir_exp ->
                Result.map
                  (fun while_ir_stmts ->
                    let new_stmt = While (string_of_int block_idx,
                                          cond_ir_exp, while_ir_stmts) in
                    (block_idx + 1, scope_stack, symtab_stack, new_stmt :: ir_stmts))
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
            let new_acc = (block_idx, new_scope_stack, new_symtab_stack, ir_stmts) in
            Result.bind (map_decl vd new_acc)
              (fun ir_dec ->
                let (block_idx, scope_stack, symtab_stack, ir_stmts) = acc in
                Result.bind (map_expr new_scope_stack new_symtab_stack cond_expr)
                  (fun cond_ir_exp ->
                    Result.bind (map_expr new_scope_stack new_symtab_stack inc_expr)
                      (fun inc_ir_exp ->
                        Result.map
                          (fun for_ir_stmts ->
                            let new_stmt = For (string_of_int block_idx,
                                                ir_dec, cond_ir_exp, inc_ir_exp,
                                                for_ir_stmts) in
                            (block_idx + 1, scope_stack, symtab_stack,
                             new_stmt :: ir_stmts))
                          (map_blk forstmts acc))))
         | _ -> Error "Error: Not a block"
       end
    | Parsetree.Continue ->
       Ok (block_idx, scope_stack, symtab_stack, Continue :: ir_stmts)
    | Parsetree.Break ->
       Ok (block_idx, scope_stack, symtab_stack, Break :: ir_stmts)
    | Parsetree.Return ret_exp_opt ->
       let ret_ir_exp_opt =
         Option.map
           (map_expr scope_stack symtab_stack)
           ret_exp_opt
       in
       let new_stmt_res = match ret_ir_exp_opt with
         | Some (Error e) -> Error e
         | Some (Ok ret_expr) -> Ok (Return (Some ret_expr))
         | None -> Ok (Return None)
       in
       Result.map
         (fun new_stmt ->
           (block_idx, scope_stack, symtab_stack, new_stmt :: ir_stmts))
         new_stmt_res

  and map_stmts stmts scope_stack symtab_stack =
    Result.map (fun (_, _, _, ir_stmts) -> ir_stmts)
      (Symtab.fold_left_bind map_stmt (0, scope_stack, symtab_stack, []) stmts)
  in

  let map_top_decl acc td = match td with
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
            Result.map (fun l -> (StaticDecl (t, "@" ^ name, l)) :: acc)
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
                            (fun ((n, _), t) -> (llvm_type_of_silktype t, "@" ^ n))
                            (List.combine args_ argtypes)
               in
               begin
                 match body with
                 | Parsetree.Block stmts ->
                    begin
                      Result.map
                        (fun ir_stmts ->
                          (FuncDecl (rt, "@" ^ name, args, ir_stmts)) :: acc)
                        (map_stmts stmts [name] [(Option.get inner_st)])
                    end
                 | _ -> Error "Error: Function body is not a block"
               end
            | _ -> Error ("Error: Symbol " ^ name ^ " is not a function")
       end
  in
  Result.map List.rev (Symtab.fold_left_bind map_top_decl [] ast)

             (* let get_type value l_type = match value with
              *   | LTemporary (t, _) -> t
              *   | LNamed (t, _) -> t
              *   | NoValue -> Void
              *   | LLiteral (Parsetree.LInt l) -> l_type
              *
              *
              * let codegen_assign =
              *   fun id (blkname, tmp_idx, insts, last_result, symtab_stack) ->
              *   let lt = match last_result with
              *     | LLiteral l ->
              *        begin
              *          match Symtab.find_symtab_stack id symtab_stack with
              *          | None -> Ok Void
              *          | Some (Symtab.Value (_, t, _)) -> llvm_type_of_silktype t
              *          | _ -> Error ("Error: Expected value, found type: " ^ id)
              *        end
              *     | _ -> Ok Void
              *   in
              *   Result.map
              *     begin
              *       fun lt ->
              *       let new_inst =
              *         (NoValue,
              *          Store (last_result,
              *                 LNamed (Pointer (get_type last_result lt), id)))
              *       in
              *       (blkname, tmp_idx, new_inst :: insts, NoValue, symtab_stack)
              *     end
              *     lt
              *
              *
              * let rec codegen_expr acc expr =
              *   let (blkname, tmp_idx, insts, last_result, symtab_stack) = acc in
              *   match expr with
              *   | Parsetree.Identifier id ->
              *      begin
              *        match Symtab.find_symtab_stack id symtab_stack with
              *        | None -> Error ("Error: Identifier " ^ id ^ " undefined")
              *        | Some (Symtab.Value (_, t, _)) ->
              *           Result.bind (llvm_type_of_silktype t)
              *             begin
              *               fun lltype ->
              *               match lltype with
              *               | Function (a, r) ->
              *                  Ok (blkname, tmp_idx, insts,
              *                      LNamed (Function (a, r), id), symtab_stack)
              *               | _ ->
              *                  let res = LTemporary (lltype, tmp_idx) in
              *                  let new_inst = (res, Load (LNamed (Pointer lltype, id))) in
              *                  Ok (blkname, tmp_idx + 1, new_inst :: insts, res, symtab_stack)
              *             end
              *        | _ -> Error ("Error: Expected value, found type: " ^ id)
              *      end
              *   | Parsetree.LLiteral l -> Ok (blkname, tmp_idx, insts, LLiteral l, symtab_stack)
              *   | Parsetree.Assignment (id, exp) ->
              *      Result.bind (codegen_expr acc exp) (fun a -> codegen_assign id a)
              *   | Parsetree.FunctionCall (fexp, args) ->
              *      Result.bind (codegen_expr acc fexp)
              *        begin
              *          fun acc ->
              *          let (_, _, _, func_value, _) = acc in
              *          let args_unresolved =
              *            Symtab.fold_left_bind
              *              begin
              *                fun (args, acc) arg_expr ->
              *                Result.map
              *                  begin
              *                    fun acc ->
              *                    let (_, _, _, arg_value, _) = acc in
              *                    (arg_value :: args, acc)
              *                  end
              *                  (codegen_expr acc arg_expr)
              *              end
              *              ([], acc) args
              *          in
              *          match  (func_value, args_unresolved) with
              *          | (LNamed (Function (argtypes, rettype), _), Ok (args_rev, acc)) ->
              *             let (blkname, tmp_idx, insts, last_result, symtab_stack) = acc in
              *             let result = LTemporary (rettype, tmp_idx) in
              *             let new_inst = (result, Call (rettype, func_value, List.rev args_rev)) in
              *             Ok (blkname, tmp_idx + 1, new_inst :: insts, result, symtab_stack)
              *          | (_, Error e) -> Error e
              *          | (_, _) -> Error "Error: Not a function"
              *        end
              *   | Parsetree.BinOp (l_expr, op, r_expr) ->
              *      Result.bind (codegen_expr acc l_expr)
              *        begin
              *          fun acc ->
              *          let (_, _, _, l_value, _) = acc in
              *          Result.bind (codegen_expr acc r_expr)
              *            begin
              *              fun acc ->
              *              let (blkname, tmp_idx, insts, r_value, ststack) = acc in
              *              (\* TODO Upcast literals as necessary. *\)
              *              let l_type = get_type l_value in
              *              let r_type = get_type r_value in
              *              match op with
              *              | Parsetree.Plus ->
              *                 let result = LTemporary (I32, tmp_idx) in
              *                 let new_inst = (result, Add (l_value, r_value)) in
              *                 Ok (blkname, tmp_idx + 1, new_inst :: insts, result, ststack)
              *              | Parsetree.LessThan ->
              *                 (\* TODO icmp *\)
              *                 let result = LTemporary (I1, tmp_idx) in
              *                 let new_inst = (result, Add (l_value, r_value)) in
              *                 Ok (blkname, tmp_idx + 1, new_inst :: insts, result, ststack)
              *            end
              *        end
              *   (\* TODO Index expressions *\)
              *   | Parsetree.Index (arr_expr, idx_expr) -> Ok acc
              *
              * let codegen_stmt acc stmt = match stmt with
              *   | Parsetree.Empty -> Ok acc
              *   | Parsetree.Decl vd ->
              *      let (id, expr) = match vd with
              *        | Parsetree.Val (id, _, expr) -> (id, expr)
              *        | Parsetree.ValI (id, expr) -> (id, expr)
              *        | Parsetree.Var (id, _, expr) -> (id, expr)
              *        | Parsetree.VarI (id, expr) -> (id, expr)
              *      in
              *      Result.bind (codegen_expr acc expr) (fun a -> codegen_assign id a)
              *   | Parsetree.Expr expr -> codegen_expr acc expr
              *   (\* TODO blocks *\)
              *   | Parsetree.Block stmts ->
              *      let (blk, tmp_idx, insts, last_result, ststack) = acc in
              *      let (blk_idx, blk_name, blk_type) = blk in
              *      Ok acc
              *   | _ -> Ok acc
              *
              * let codegen_func name stmts st =
              *   let init_acc = ((0, name, Block), 0, [], NoValue, st) in
              *   Symtab.fold_left_bind codegen_stmt init_acc stmts
              *
              * let codegen_ast ast =
              *   Result.bind (Symtab.construct_symtab ast)
              *     begin
              *       fun symtab ->
              *       let codegen_top_decl acc decl = match decl with
              *         | Parsetree.TypeDef _ -> Ok acc
              *         | Parsetree.ValDecl _ -> Ok acc
              *         | Parsetree.FuncDecl (name, _, _, Parsetree.Block stmts) ->
              *            Result.map
              *              (fun (_, _, insts, _, _) -> insts @ acc)
              *              (codegen_func name stmts [symtab])
              *         | _ -> Error "Error: Code generation failed"
              *       in
              *       Symtab.fold_left_bind codegen_top_decl [] ast
              *     end
              *
              * let serialize_code code =
              *   let rec string_of_llvm_type t = match t with
              *     | I32 -> "i32"
              *     | Pointer t -> (string_of_llvm_type t) ^ "*"
              *     | Void -> "void"
              *     | _ -> "<type>"
              *   in
              *   let string_of_val lv = match lv with
              *     | LTemporary (_, i) -> string_of_int i
              *     | LNamed (_, n) -> n
              *     | LLiteral (Parsetree.LInt l) -> string_of_int l
              *     | _ -> "<lval>"
              *   in
              *   let serialize_inst inst = match inst with
              *     | (lv, Alloca t) ->
              *        ((string_of_val lv) ^ " = alloca " ^ (string_of_llvm_type t))
              *     | (lv, Load v) ->
              *        let vt = string_of_llvm_type (get_type v Void) in
              *        let lvt = string_of_llvm_type (get_type lv Void) in
              *        let vstr = string_of_val v in
              *        let lvstr = string_of_val lv in
              *        (lvstr ^ " = load " ^ lvt ^ ", " ^ vt ^ " " ^ vstr)
              *     | (lv, Store (a, b)) ->
              *        let at = string_of_llvm_type (get_type a Void) in
              *        let bt = string_of_llvm_type (get_type b Void) in
              *        ("store " ^ at ^ " " ^ (string_of_val a) ^ ", " ^ bt ^ " " ^ (string_of_val b))
              *     | (lv, Add (a, b)) ->
              *        let t = string_of_llvm_type (get_type lv Void) in
              *        let lvstr = string_of_val lv in
              *        let astr = string_of_val a in
              *        let bstr = string_of_val b in
              *        (lvstr ^ " = add " ^ t ^ " " ^ astr ^ ", " ^ bstr)
              *     | _ -> "nop"
              *   in
              *   List.rev (List.map serialize_inst code) *)
