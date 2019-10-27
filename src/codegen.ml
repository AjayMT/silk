
(*
 * LLVM Code generation.
 *)

type llvm_type = I1 | I8 | I16 | I32 | I64
                 | Pointer of llvm_type
                 | Function of llvm_type list * llvm_type
                 | Void

type llvm_value = Temporary of llvm_type * int
                | Named of llvm_type * string
                | BlockLabel of int * string * string
                | Literal of Parsetree.literal
                | NoValue

type llvm_inst = Alloca of llvm_type
               | Load of llvm_value
               | Store of llvm_value * llvm_value
               | Call of llvm_type * llvm_value * llvm_value list
               | Ret of llvm_value
               | Add of llvm_value * llvm_value
               | Empty

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

let rec codegen_expr acc expr =
  let (blkname, tmp_idx, insts, last_result, symtab_stack) = acc in
  match expr with
  | Parsetree.Identifier id ->
     begin
       match Symtab.find_symtab_stack id symtab_stack with
       | None -> Error ("Error: Identifier " ^ id ^ " undefined")
       | Some t ->
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
     end
  | Parsetree.Literal l -> Ok (blkname, tmp_idx, insts, Literal l, symtab_stack)
  | Parsetree.Assignment (id, exp) ->
     Result.bind (codegen_expr acc exp)
       begin
         fun (blkname, tmp_idx, insts, last_result, symtab_stack) ->
         (* TODO cast literals as necessary. *)
         let new_inst = (NoValue, Store (last_result, Named (Void, id))) in
         Ok (blkname, tmp_idx, new_inst :: insts, NoValue, symtab_stack)
       end
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
     let get_type value = match value with
       | Temporary (t, _) -> t
       | Named (t, _) -> t
       | BlockLabel _ -> Void
       | NoValue -> Void
       | Literal (Parsetree.LInt l) -> I32
     in

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
  | _ -> Ok acc

let codegen_func name stmts =
  Symtab.fold_left_bind codegen_stmt (Ok (name, 0, [])) stmts
