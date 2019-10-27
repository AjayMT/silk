
(*
 * LLVM Code generation.
 *)

type block_type = Block | IfElse | For | While

type llvm_type = I1 | I8 | I16 | I32 | I64
                 | U1 | U8 | U16 | U32 | U64
                 | Pointer of llvm_type
                 | Function of llvm_type list * llvm_type
                 | Void

type llvm_value = Temporary of llvm_type * int
                | Named of llvm_type * string
                | Literal of Parsetree.literal
                | NoValue

type llvm_inst = Alloca of llvm_type
               | Load of llvm_value
               | Store of llvm_value * llvm_value
               | Call of llvm_type * llvm_value * llvm_value list
               | Ret of llvm_value
               | Add of llvm_value * llvm_value
               | Label of string * llvm_inst
               | Empty

let get_type value l_type = match value with
  | Temporary (t, _) -> t
  | Named (t, _) -> t
  | NoValue -> Void
  | Literal (Parsetree.LInt l) -> l_type

let rec llvm_type_of_silktype t =
  match t with
  | Symtab.Function (argtypes, rettype) ->
     let llargtypes = Result.map
                        List.rev
                        (Symtab.fold_left_bind
                           (fun acc t -> Result.map
                                           (fun t -> t :: acc)
                                           (llvm_type_of_silktype t))
                           [] argtypes)
     in
     let llrettype = llvm_type_of_silktype rettype in
     begin
       match (llargtypes, llrettype) with
       | (Error e, _) -> Error e
       | (_, Error e) -> Error e
       | (Ok llat, Ok llrt) -> Ok (Function (llat, llrt))
     end
  | Symtab.Bool -> Ok I8
  | Symtab.Int -> Ok I32
  | Symtab.Void -> Ok Void
  | Symtab.NewType (_, t) -> llvm_type_of_silktype t

let codegen_assign =
  fun id (blkname, tmp_idx, insts, last_result, symtab_stack) ->
  let lt = match last_result with
    | Literal l ->
       begin
         match Symtab.find_symtab_stack id symtab_stack with
         | None -> Ok Void
         | Some (Symtab.Value (_, t, _)) -> llvm_type_of_silktype t
         | _ -> Error ("Error: Expected value, found type: " ^ id)
       end
    | _ -> Ok Void
  in
  Result.map
    begin
      fun lt ->
      let new_inst =
        (NoValue,
         Store (last_result,
                Named (Pointer (get_type last_result lt), id)))
      in
      (blkname, tmp_idx, new_inst :: insts, NoValue, symtab_stack)
    end
    lt


let rec codegen_expr acc expr =
  let (blkname, tmp_idx, insts, last_result, symtab_stack) = acc in
  match expr with
  | Parsetree.Identifier id ->
     begin
       match Symtab.find_symtab_stack id symtab_stack with
       | None -> Error ("Error: Identifier " ^ id ^ " undefined")
       | Some (Symtab.Value (_, t, _)) ->
          Result.bind (llvm_type_of_silktype t)
            begin
              fun lltype ->
              match lltype with
              | Function (a, r) ->
                 Ok (blkname, tmp_idx, insts,
                     Named (Function (a, r), id), symtab_stack)
              | _ ->
                 let res = Temporary (lltype, tmp_idx) in
                 let new_inst = (res, Load (Named (Pointer lltype, id))) in
                 Ok (blkname, tmp_idx + 1, new_inst :: insts, res, symtab_stack)
            end
       | _ -> Error ("Error: Expected value, found type: " ^ id)
     end
  | Parsetree.Literal l -> Ok (blkname, tmp_idx, insts, Literal l, symtab_stack)
  | Parsetree.Assignment (id, exp) ->
     Result.bind (codegen_expr acc exp) (fun a -> codegen_assign id a)
  | Parsetree.FunctionCall (fexp, args) ->
     Result.bind (codegen_expr acc fexp)
       begin
         fun acc ->
         let (_, _, _, func_value, _) = acc in
         let args_unresolved =
           Symtab.fold_left_bind
             begin
               fun (args, acc) arg_expr ->
               Result.map
                 begin
                   fun acc ->
                   let (_, _, _, arg_value, _) = acc in
                   (arg_value :: args, acc)
                 end
                 (codegen_expr acc arg_expr)
             end
             ([], acc) args
         in
         match  (func_value, args_unresolved) with
         | (Named (Function (argtypes, rettype), _), Ok (args_rev, acc)) ->
            let (blkname, tmp_idx, insts, last_result, symtab_stack) = acc in
            let result = Temporary (rettype, tmp_idx) in
            let new_inst = (result, Call (rettype, func_value, List.rev args_rev)) in
            Ok (blkname, tmp_idx + 1, new_inst :: insts, result, symtab_stack)
         | (_, Error e) -> Error e
         | (_, _) -> Error "Error: Not a function"
       end
  | Parsetree.BinOp (l_expr, op, r_expr) ->
     Result.bind (codegen_expr acc l_expr)
       begin
         fun acc ->
         let (_, _, _, l_value, _) = acc in
         Result.bind (codegen_expr acc r_expr)
           begin
             fun acc ->
             let (blkname, tmp_idx, insts, r_value, ststack) = acc in
             (* TODO Upcast literals as necessary. *)
             let l_type = get_type l_value in
             let r_type = get_type r_value in
             match op with
             | Parsetree.Plus ->
                let result = Temporary (I32, tmp_idx) in
                let new_inst = (result, Add (l_value, r_value)) in
                Ok (blkname, tmp_idx + 1, new_inst :: insts, result, ststack)
             | Parsetree.LessThan ->
                (* TODO icmp *)
                let result = Temporary (I1, tmp_idx) in
                let new_inst = (result, Add (l_value, r_value)) in
                Ok (blkname, tmp_idx + 1, new_inst :: insts, result, ststack)
           end
       end
  (* TODO Index expressions *)
  | Parsetree.Index (arr_expr, idx_expr) -> Ok acc

let codegen_stmt acc stmt = match stmt with
  | Parsetree.Empty -> Ok acc
  | Parsetree.Decl vd ->
     let (id, expr) = match vd with
       | Parsetree.Val (id, _, expr) -> (id, expr)
       | Parsetree.ValI (id, expr) -> (id, expr)
       | Parsetree.Var (id, _, expr) -> (id, expr)
       | Parsetree.VarI (id, expr) -> (id, expr)
     in
     Result.bind (codegen_expr acc expr) (fun a -> codegen_assign id a)
  | Parsetree.Expr expr -> codegen_expr acc expr
  (* TODO blocks *)
  | Parsetree.Block stmts ->
     let (blk, tmp_idx, insts, last_result, ststack) = acc in
     let (blk_idx, blk_name, blk_type) = blk in
     Ok acc
  | _ -> Ok acc

let codegen_func name stmts st =
  let init_acc = ((0, name, Block), 0, [], NoValue, st) in
  Symtab.fold_left_bind codegen_stmt init_acc stmts

let codegen_ast ast =
  Result.bind (Symtab.construct_symtab ast)
    begin
      fun symtab ->
      let codegen_top_decl acc decl = match decl with
        | Parsetree.TypeDef _ -> Ok acc
        | Parsetree.ValDecl _ -> Ok acc
        | Parsetree.FuncDecl (name, _, _, Parsetree.Block stmts) ->
           Result.map
             (fun (_, _, insts, _, _) -> insts @ acc)
             (codegen_func name stmts [symtab])
        | _ -> Error "Error: Code generation failed"
      in
      Symtab.fold_left_bind codegen_top_decl [] ast
    end

let serialize_code code =
  let rec string_of_llvm_type t = match t with
    | I32 -> "i32"
    | Pointer t -> (string_of_llvm_type t) ^ "*"
    | Void -> "void"
    | _ -> "<type>"
  in
  let string_of_val lv = match lv with
    | Temporary (_, i) -> string_of_int i
    | Named (_, n) -> n
    | Literal (Parsetree.LInt l) -> string_of_int l
    | _ -> "<lval>"
  in
  let serialize_inst acc inst = match inst with
    | (lv, Alloca t) ->
       ((string_of_val lv) ^ " = alloca " ^ (string_of_llvm_type t)) :: acc
    | (lv, Load v) ->
       let vt = string_of_llvm_type (get_type v Void) in
       let lvt = string_of_llvm_type (get_type lv Void) in
       let vstr = string_of_val v in
       let lvstr = string_of_val lv in
       (lvstr ^ " = load " ^ lvt ^ ", " ^ vt ^ " " ^ vstr) :: acc
    | (lv, Store (a, b)) ->
       let at = string_of_llvm_type (get_type a Void) in
       let bt = string_of_llvm_type (get_type b Void) in
       ("store " ^ at ^ " " ^ (string_of_val a) ^ ", " ^ bt ^ " " ^ (string_of_val b)) :: acc
    | (lv, Add (a, b)) ->
       let t = string_of_llvm_type (get_type lv Void) in
       let lvstr = string_of_val lv in
       let astr = string_of_val a in
       let bstr = string_of_val b in
       (lvstr ^ " = add " ^ t ^ " " ^ astr ^ ", " ^ bstr) :: acc
    | _ -> acc
  in
  List.fold_left serialize_inst [] code
