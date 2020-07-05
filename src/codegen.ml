
(**
 * LLVM Code generation.
 *)

module ScopeM = Map.Make(String)

(**
 * LLVM code generation is performed in two stages:
 *   1. Conversion of the AST + symbol table into an "IR" (intermediate
 *      representation) tree
 *   2. Conversion of the IR tree into a list of llvm_insts (instructions)
 *      which are serialized into LLVM code.
 *
 * The IR tree (IRT) is structurally similar to the AST but includes all type
 * information needed to produce LLVM instructions. In constructing the IRT,
 * we unify the AST and symbol table data structures, remove extraneous
 * information (distinction between mutable and immutable pointers, array and
 * struct member access, etc.) and simplify some expressions (boolean binops,
 * type casts and others).
 *
 * The IRT is traversed to produce llvm_insts, which generally map directly to
 * LLVM instructions. llvm_insts act on llvm_values, which can be literals,
 * named values or temporaries (also 'undef' and 'zeroinitializer' which are
 * special literals).
 *)

type llvm_type = I of int
               | U of int
               | F of int
               | Pointer of llvm_type
               | Array of int * llvm_type
               | Struct of bool * llvm_type list
               | StructLabeled of bool * (string * llvm_type) list
               | Function of llvm_type list * llvm_type
               | OpaqueType of string * string
               | Alias of llvm_type * string
               | Void

type ir_literal = Int of int | Float of float | String_ of string
                  | GlobalString of string
type ir_bin_op = Plus | Minus | Times | Divide | Modulus
                 | Equal | LessThan | GreaterThan
                 | And | Or
                 | RShift | LShift | Xor
type ir_un_op = UMinus | Not | AddressOf | Deref
type ir_expr = Identifier of llvm_type * string
             | ParamIdentifier of llvm_type * string
             | Literal of llvm_type * ir_literal
             | StructLiteral of llvm_type * llvm_type * ir_expr list
             | ArrayElems of llvm_type * ir_expr list
             | ArrayInit of llvm_type
             | Assignment of llvm_type * string * ir_expr
             | Write of llvm_type * ir_expr * ir_expr
             | FunctionCall of llvm_type * ir_expr * llvm_type list * ir_expr list
             | BinOp of llvm_type * ir_bin_op * ir_expr * ir_expr
             | UnOp of llvm_type * ir_un_op * ir_expr
             | ItoF of llvm_type * ir_expr * llvm_type
             | FtoI of llvm_type * ir_expr * llvm_type
             | BitCast of llvm_type * ir_expr * llvm_type
             | PtoI of llvm_type * ir_expr * llvm_type
             | ItoP of llvm_type * ir_expr * llvm_type
             | Trunc of llvm_type * ir_expr * llvm_type
             | Ext of llvm_type * ir_expr * llvm_type
             | StructAccess of llvm_type * ir_expr * int
             | GetElemPtr of llvm_type * llvm_type * ir_expr
                             * (llvm_type * ir_expr) list
             | StructAssign of llvm_type * ir_expr * ir_expr list
             | Temporary of llvm_type
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
             | Return of llvm_type * ir_expr option
type ir_root = StaticDecl of llvm_type * bool * string * ir_literal
             | FuncDecl of llvm_type * bool * string
                           * (llvm_type * string) list * ir_stmt list
             | FuncFwdDecl of llvm_type * string
                              * (llvm_type * string) list * bool
             | TypeDef of llvm_type * string

type llvm_value = LTemporary of int
                | LNamed of string
                | LLiteral of ir_literal
                | LZeroInit
                | LUndef
                | NoValue
type llvm_inst = Alloca of llvm_type
               | Load of llvm_type * llvm_type * llvm_value
               | GEP of llvm_type * llvm_type * llvm_value
                        * (llvm_type * llvm_value) list
               | InsertValue of llvm_type * llvm_value * llvm_type * llvm_value
                                * int
               | ExtractValue of llvm_type * llvm_value * int
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
               | PtrToInt of llvm_type * llvm_value * llvm_type
               | IntToPtr of llvm_type * llvm_value * llvm_type
               | Trunc of llvm_type * llvm_value * llvm_type
               | Ext of llvm_type * llvm_value * llvm_type

               | NoInst


let ( let* ) x f = Result.bind x f
let ( let+ ) x f = Result.map f x


(**
 * llvm_type_of_silktype converts a silktype (used in the symtab)
 * to an llvm_type (used in the IRT). It preserves type aliases
 * and newtypes to allow forward-declaration and because LLVM
 * treats all anonymous structure types as unique (even if they
 * are equivalent).
 *)

let rec llvm_type_of_silktype t =
  match t with
  | Symtab.Function (argtypes, rettype) ->
     let llargtypes = List.map llvm_type_of_silktype argtypes in
     let llrettype = llvm_type_of_silktype rettype in
     Function (llargtypes, llrettype)
  | Symtab.Bool -> I 1
  | Symtab.I i  -> I i
  | Symtab.U u  -> U u
  | Symtab.F f  -> F f
  | Symtab.Void -> Void
  | Symtab.Pointer t | Symtab.MutPointer t ->
     Pointer (llvm_type_of_silktype t)
  | Symtab.Array (len, t) ->
     Array (len, llvm_type_of_silktype t)
  | Symtab.TypeAlias (n, t) ->
     begin match t with
     | Symtab.TypeAlias (n, t) ->
        Alias (llvm_type_of_silktype t, "%" ^ "\"" ^ n ^ "\"")
     | _ ->
        Alias (llvm_type_of_silktype t, "%" ^ "\"" ^ n ^ "\"")
     end
  | Symtab.Struct (packed, types) ->
     let types = List.map llvm_type_of_silktype types in
     Struct (packed, types)
  | Symtab.StructLabeled (packed, pairs) ->
     let (names, types) = List.split pairs in
     let types = List.map llvm_type_of_silktype types in
     StructLabeled (packed, List.combine names types)
  | Symtab.TypeStub n -> OpaqueType ("%" ^ "\"" ^ n ^ "\"", n)


(**
 * discard_labels converts a labeled struct type into an unlabeled
 * struct type by removing the labels.
 *)

let discard_labels t = match t with
  | StructLabeled (packed, pairs) ->
     let (_, types) = List.split pairs in Struct (packed, types)
  | _ -> t


(**
 * resolve_alias finds the base type of a type alias or newtype.
 * When resolving a type stub it searches the symbol table.
 *)

let rec resolve_alias types_tab_stack t = match t with
  | Alias (t, _) -> resolve_alias types_tab_stack t
  | OpaqueType (_, n) ->
     begin match Symtab.find_symtab_stack n types_tab_stack with
     | Some (Symtab.Type t) ->
        resolve_alias types_tab_stack (llvm_type_of_silktype t)
     | _ -> Error ("Error: Failed to resolve type '" ^ n ^ "'")
     end
  | _ -> Ok t


(**
 * resolve_literal evaluates a literal (AST) expression or
 * expression containing only literals. This is used to validate
 * and simplify top-level declarations.
 *)

(* TODO complex literals *)
let rec resolve_literal l =
  let err = Error ("Error: Could not evaluate expression '"
                   ^ (Parsetree.show_expr l) ^ "' at compile time")
  in
  match l with
  | Parsetree.Literal l ->
     begin match l with
     | Parsetree.LI8  i -> Ok (Int i)
     | Parsetree.LI16 i -> Ok (Int i)
     | Parsetree.LI32 i -> Ok (Int i)
     | Parsetree.LI64 i -> Ok (Int i)
     | Parsetree.LU8  i -> Ok (Int i)
     | Parsetree.LU16 i -> Ok (Int i)
     | Parsetree.LU32 i -> Ok (Int i)
     | Parsetree.LU64 i -> Ok (Int i)
     | Parsetree.LF32 f -> Ok (Float f)
     | Parsetree.LF64 f -> Ok (Float f)
     | Parsetree.LBool b -> Ok (Int (if b then 1 else 0))
     | Parsetree.LString s -> Ok (GlobalString s)
     end
  | Parsetree.BinOp (l, op, r) ->
     let* l = resolve_literal l in
     let* r = resolve_literal r in
     let (intf, floatf, boolf) = match op with
       | Parsetree.Plus -> (Ok (+), Ok (+.), err)
       | Parsetree.Minus -> (Ok (-), Ok (-.), err)
       | Parsetree.Times -> (Ok ( * ), Ok ( *. ), err)
       | Parsetree.Divide -> (Ok (/), Ok (/.), err)
       | Parsetree.Modulus -> (Ok (mod), err, err)
       | Parsetree.Equal -> (err, err, Ok (=))
       | Parsetree.LessThan -> (err, err, Ok (<))
       | Parsetree.GreaterThan -> (err, err, Ok (>))
       | Parsetree.And | Parsetree.BitAnd -> (Ok (land), err, err)
       | Parsetree.Or | Parsetree.BitOr -> (Ok (lor), err, err)
       | Parsetree.BitXor -> (Ok (lxor), err, err)
       | Parsetree.LShift -> (Ok (lsl), err, err)
       | Parsetree.RShift -> (Ok (lsr), err, err) (* TODO arithmetic shift right? *)
     in
     begin match (l, r, boolf) with
     | (Int a, Int b, Error _) -> let+ intf = intf in Int (intf a b)
     | (Float a, Float b, Error _) ->  let+ floatf = floatf in Float (floatf a b)
     | (Int a, Int b, Ok boolf) -> Ok (Int (if boolf a b then 1 else 0))
     | (Float a, Float b, Ok boolf) -> Ok (Float (if boolf a b then 1.0 else 0.0))
     | _ -> err
     end
  | Parsetree.UnOp (op, expr) ->
     let* expr = resolve_literal expr in
     let (intf, floatf, boolf) = match op with
       | Parsetree.UMinus -> (Ok ((-) 0), Ok ((-.) 0.0), err)
       | Parsetree.Not -> (err, err, Ok (not))
       | Parsetree.BitNot -> (Ok (lnot), err, err)
       | _ -> (err, err, err)
     in
     begin match (expr, boolf) with
     | (Int a, Error _) -> let+ intf = intf in Int (intf a)
     | (Float a, Error _) -> let+ floatf = floatf in Float (floatf a)
     | (Int a, Ok boolf) ->
        let r = boolf (if a = 0 then false else true) in
        Ok (Int (if r then 1 else 0))
     | _ -> err
     end
  | _ -> err


(**
 * eval_ir_expr_type evaluates the type of an IRT expression.
 * Since IRT nodes carry type information, this is much simpler than
 * evaluating the type of an AST node (which is done in eval_expr_type
 * in symtab.ml). The function uses resolve_alias, which requires the
 * symbol table to resolve type stubs.
 *)

let eval_ir_expr_type types_tab_stack ir_exp = match ir_exp with
  | Identifier (t, _) -> Ok t
  | ParamIdentifier (t, _) -> Ok t
  | Literal (t, _) -> Ok t
  | Assignment (t, _, _) -> Ok t
  | Write (t, _, _) -> Ok t
  | FunctionCall (t, _, _, _) ->
     begin match t with
     | Function (_, rt) -> Ok rt
     | _ -> Error "Error: Cannot call non-function type"
     end
  | BinOp (t, o, _, _) ->
     begin match o with
     | Equal | GreaterThan | LessThan -> Ok (I 1)
     | _ -> Ok t (* all other binops have the same operand and result types *)
     end
  | ItoF (_, _, t) -> Ok t
  | FtoI (_, _, t) -> Ok t
  | BitCast (_, _, t) -> Ok t
  | PtoI (_, _, t) -> Ok t
  | ItoP (_, _, t) -> Ok t
  | Trunc (_, _, t) -> Ok t
  | Ext (_, _, t) -> Ok t
  | ArrayElems (t, _) -> Ok t
  | ArrayInit t -> Ok t
  | Temporary t -> Ok t
  | StructAssign (t, _, _) -> Ok t
  | StructLiteral (t, _, _) -> Ok t
  | StructAccess (t, exp, i) ->
     let* t = resolve_alias types_tab_stack t in
     begin match t with
     | Struct (_, l) -> Ok (List.nth l i)
     | StructLabeled (_, l) -> Ok ((fun (_, t) -> t) @@ List.nth l i)
     | _ -> Error "Error: Cannot access member of non-struct type"
     end
  | GetElemPtr (_, pt, _, idxs) ->
     let get_elem_type t _ =
       let* t = resolve_alias types_tab_stack t in
       match t with
       | Array (_, t) -> Ok t
       | Pointer t -> Ok t
       | _ -> Error "Error: Cannot dereference non-pointer type"
     in
     let+ elem_type = Util.flb get_elem_type pt idxs in
     Pointer elem_type
  | UnOp (t, op, _) ->
     match op with
     | AddressOf -> Ok (Pointer t)
     | Deref ->
        let* t = resolve_alias types_tab_stack t in
        begin match t with
        | Pointer s -> Ok s
        | _ -> Error "Error: Cannot dereference non-pointer type"
        end
     | _ -> Ok t (* uminus and not have the same operand and result types *)


(**
 * construct_ir_tree converts the AST and symbol table into the IRT.
 * In doing so, it simplifies some expressions and checks for invalid
 * continue/break statements. It also 'de-scopes ' all symbols.
 *)

let construct_ir_tree ast symtab =

  (**
   * The 'scope' of every symbol is the stack of nested symbol tables
   * that have been traversed to reach that symbol (see comments above
   * find_symtab_stack in symtab.ml). Symtabs/scopes can be associated
   * with functions or blocks; function scopes are identified with the
   * name of the function, block scopes are identified with the 'index'
   * of the block (see construct_block_symtab in symtab.ml).
   *)
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
    let types_tab_stack = symtab_stack @ [symtab] in
    match expr with
    | Parsetree.Identifier name ->
       let symbol = find_symtab_stack name symtab_stack in
       begin match symbol with
       | Some (Symtab.Type _) ->
          Error ("Error: Expected value, found type '" ^ name
                 ^ "' in expression '" ^ (Parsetree.show_expr expr) ^ "'")
       | Some (Symtab.Value (_, type_, _)) ->
          let t = llvm_type_of_silktype type_ in
          Ok (Identifier (t, ScopeM.find name scope_map))
       | None -> Error ("Error: Identifier '" ^ name ^ "' undefined in expression '"
                        ^ (Parsetree.show_expr expr) ^ "'")
       end

    | Parsetree.TemplateInstance (name, types) ->
       Error ("Error: Failed to instantiate template in expression '" ^
                (Parsetree.show_expr expr) ^ "'")

    | Parsetree.Literal l ->
       begin match l with
       | Parsetree.LI8  i -> Ok (Literal (I 8, Int i))
       | Parsetree.LI16 i -> Ok (Literal (I 16, Int i))
       | Parsetree.LI32 i -> Ok (Literal (I 32, Int i))
       | Parsetree.LI64 i -> Ok (Literal (I 64, Int i))
       | Parsetree.LU8  i -> Ok (Literal (U 8, Int i))
       | Parsetree.LU16 i -> Ok (Literal (U 16, Int i))
       | Parsetree.LU32 i -> Ok (Literal (U 32, Int i))
       | Parsetree.LU64 i -> Ok (Literal (U 64, Int i))
       | Parsetree.LF32 f -> Ok (Literal (F 32, Float f))
       | Parsetree.LF64 f -> Ok (Literal (F 64, Float f))
       | Parsetree.LBool b -> Ok (Literal (I 1, Int (if b then 1 else 0)))
       | Parsetree.LString s -> Ok (Literal (Pointer (I 8), String_ s))
       end

    | Parsetree.Assignment (lval, exp) ->
       let* rexp = map_expr scope_map symtab_stack exp in
       let rec resolve_assignment lval rexp =
         let* rexp_type = eval_ir_expr_type types_tab_stack rexp in
         match lval with
         | Parsetree.Identifier (name) ->
            let symbol = find_symtab_stack name symtab_stack in
            begin match symbol with
            | Some (Symtab.Type _) ->
               Error ("Error: Expected value, found type '" ^ name
                      ^ "' in expression '" ^ (Parsetree.show_expr expr) ^ "'")
            | Some (Symtab.Value (_, type_, _)) ->
               let t = llvm_type_of_silktype type_ in
               Ok (Assignment (t, ScopeM.find name scope_map, rexp))
            | None ->
               Error ("Error: Identifier '" ^ name ^ "' undefined in expression '"
                      ^ (Parsetree.show_expr expr) ^ "'")
            end
         | Parsetree.UnOp (Parsetree.Deref, lexp) ->
            let+ lexp = map_expr scope_map symtab_stack lexp in
            Write (rexp_type, lexp, rexp)
         | Parsetree.Index (array_expr, idx_expr) ->
            let* array = map_expr scope_map symtab_stack array_expr in
            let* idx = map_expr scope_map symtab_stack idx_expr in
            let* idx_type = eval_ir_expr_type types_tab_stack idx in
            let+ array_type = eval_ir_expr_type types_tab_stack array in
            let ptr_exp =
              GetElemPtr (array_type, Pointer array_type,
                          UnOp (array_type, AddressOf, array),
                          [(I 32, Literal (I 32, Int 0)); (idx_type, idx)])
            in
            Write (rexp_type, ptr_exp, rexp)
         | Parsetree.StructIndexAccess _ | Parsetree.StructMemberAccess _ ->
            let* lval = map_expr scope_map symtab_stack lval in
            let+ lval_type = eval_ir_expr_type types_tab_stack lval in
            Write (rexp_type, UnOp (lval_type, AddressOf, lval), rexp)
         | Parsetree.StructLiteral (_, exprs) ->
            let* rexp_type_resolved = resolve_alias types_tab_stack rexp_type in
            let* members = match rexp_type_resolved with
              | Struct (_, l) -> Ok l
              | StructLabeled (_, l) -> Ok ((fun (_, t) -> t) @@ List.split l)
              | _ -> Error ("Error: Mismatched type of lval '"
                            ^ (Parsetree.show_expr lval)
                            ^ "' in struct assignment in expression '"
                            ^ (Parsetree.show_expr expr) ^ "'")

            in
            let tmp = Temporary rexp_type in
            let add_assign acc exp =
              let (acc, idx) = acc in
              let+ expr =
                (resolve_assignment exp (StructAccess (rexp_type, tmp, idx)))
              in
              (expr :: acc, idx + 1)
            in
            let+ (exprs, _) = Util.flb add_assign ([], 0) exprs in
            StructAssign (rexp_type, rexp, exprs)
         | _ -> Error ("Error: Invalid lvalue expression '"
                       ^ (Parsetree.show_expr lval) ^ "' in expression '"
                       ^ (Parsetree.show_expr expr) ^ "'")
       in
       resolve_assignment lval rexp

    | Parsetree.TypeCast (type_, expr) ->
       let* silktype = Symtab.silktype_of_asttype types_tab_stack type_ in
       let* ir_expr = map_expr scope_map symtab_stack expr in
       let casttype = llvm_type_of_silktype silktype in
       let+ ir_expr_type = eval_ir_expr_type types_tab_stack ir_expr in
       let rec resolve_cast casttype ir_expr_type = match casttype with
         | F f ->
            begin match ir_expr_type with
            | I _ | U _ -> ItoF (ir_expr_type, ir_expr, casttype)
            | F g ->
               begin match (f, g) with
               | (f, g) when f > g -> Ext (ir_expr_type, ir_expr, casttype)
               | (f, g) when g > f -> Trunc (ir_expr_type, ir_expr, casttype)
               | _ -> ir_expr
               end
            | Alias (t, _) -> resolve_cast casttype t
            | _ -> ir_expr
            end
         | I 1 ->
            let zerol = match ir_expr_type with
              | F _ -> Float 0.0
              | _ -> Int 0
            in
            UnOp (I 1, Not,
                  BinOp (ir_expr_type, Equal, ir_expr, Literal (ir_expr_type, zerol)))
         | I i | U i ->
            begin match ir_expr_type with
            | F _ -> FtoI (ir_expr_type, ir_expr, casttype)
            | Pointer t -> PtoI (ir_expr_type, ir_expr, casttype)
            | I j | U j ->
               begin match (i, j) with
               | (i, j) when i > j -> Ext (ir_expr_type, ir_expr, casttype)
               | (i, j) when j > i -> Trunc (ir_expr_type, ir_expr, casttype)
               | _ -> ir_expr
               end
            | Alias (t, _) -> resolve_cast casttype t
            | _ -> ir_expr
            end
         | Pointer t ->
            begin match ir_expr_type with
            | I _ | U _ -> ItoP (ir_expr_type, ir_expr, casttype)
            | Pointer s when t <> s -> BitCast (ir_expr_type, ir_expr, casttype)
            | Alias (t, _) -> resolve_cast casttype t
            | _ -> ir_expr
            end
         | Alias (t, _) -> resolve_cast t ir_expr_type
         | _ -> BitCast (ir_expr_type, ir_expr, casttype)
       in
       resolve_cast casttype ir_expr_type

    | Parsetree.FunctionCall (fexp, argexps) ->
       let map_ir_fexp ir_fexp =
         let* ir_fexp_type = eval_ir_expr_type types_tab_stack ir_fexp in
         match ir_fexp_type with
         | Function (argtypes, rettype) ->
            let+ args =
              Util.map_join (map_expr scope_map symtab_stack) argexps
            in
            FunctionCall (ir_fexp_type, ir_fexp, argtypes, args)
         | _ -> Error ("Error: Expression '" ^ (Parsetree.show_expr fexp)
                       ^ "' is not a function in expression '"
                       ^ (Parsetree.show_expr expr) ^ "'")
       in
       begin match map_expr scope_map symtab_stack fexp with
       | Ok e -> map_ir_fexp e
       | Error e ->
          let rec process_fexp f = match f with
            | Parsetree.Identifier t ->
               let* silkt =
                 Symtab.silktype_of_asttype types_tab_stack (Parsetree.TypeAlias t)
               in
               let* type_ =
                 resolve_alias types_tab_stack @@ llvm_type_of_silktype silkt
               in
               let expr = match type_ with
                 | Struct _ | StructLabeled _ ->
                    Parsetree.StructInit (Parsetree.TypeAlias t, argexps)
                 | _ -> Parsetree.TypeCast (Parsetree.TypeAlias t, List.hd argexps)
               in
               map_expr scope_map symtab_stack expr
            | _ -> Error e
          in process_fexp fexp
       end

    | Parsetree.BinOp (lexp_, op_, rexp_) ->
       let* l = map_expr scope_map symtab_stack lexp_ in
       let* r = map_expr scope_map symtab_stack rexp_ in
       let* l_type = eval_ir_expr_type types_tab_stack l in
       let+ r_type = eval_ir_expr_type types_tab_stack r in
       let ptr = match (l_type, r_type) with
         | (Pointer _, _) -> Some (l_type, l, r_type, r)
         | (_, Pointer _) -> Some (r_type, r, l_type, l)
         | _ -> None
       in
       begin match op_ with
       | Parsetree.Plus ->
          begin match ptr with
          | Some (Pointer p, ptr_value, other_type, other_value) ->
             GetElemPtr (p, Pointer p, ptr_value, [other_type, other_value])
          | _ -> BinOp (l_type, Plus, l, r)
          end
       | Parsetree.Minus ->
          begin match ptr with
          | Some (Pointer p, ptr_value, other_type, other_value) ->
             GetElemPtr (p, Pointer p, ptr_value,
                         [other_type, UnOp (other_type, UMinus, other_value)])
          | _ -> BinOp (l_type, Minus, l, r)
          end

       | Parsetree.Times -> BinOp (l_type, Times, l, r)
       | Parsetree.Divide -> BinOp (l_type, Divide, l, r)
       | Parsetree.Modulus -> BinOp (l_type, Modulus, l, r)

       | Parsetree.Equal -> BinOp (l_type, Equal, l, r)
       | Parsetree.LessThan -> BinOp (l_type, LessThan, l, r)
       | Parsetree.GreaterThan ->
          BinOp (l_type, GreaterThan, l, r)

       | Parsetree.And | Parsetree.BitAnd ->
          BinOp (l_type, And, l, r)
       | Parsetree.Or | Parsetree.BitOr ->
          BinOp (l_type, Or, l, r)
       | Parsetree.BitXor -> BinOp (l_type, Xor, l, r)

       | Parsetree.LShift -> BinOp (l_type, LShift, l, r)
       | Parsetree.RShift -> BinOp (l_type, RShift, l, r)
       end
    | Parsetree.UnOp (op, expr) ->
       let* ir_expr = map_expr scope_map symtab_stack expr in
       let o = match op with
         | Parsetree.UMinus -> UMinus
         | Parsetree.Not | Parsetree.BitNot -> Not
         | Parsetree.Deref -> Deref
         | Parsetree.AddressOf -> AddressOf
       in
       let+ t = eval_ir_expr_type types_tab_stack ir_expr in
       begin match (o, ir_expr) with
       | (AddressOf, UnOp (_, Deref, exp)) -> exp
       | _ -> UnOp (t, o, ir_expr)
       end

    | Parsetree.StructLiteral (packed, elems) ->
       let check_elem acc elem =
         let+ exp = map_expr scope_map symtab_stack elem in
         exp :: acc
       in
       let* elems = Util.flb check_elem [] elems in
       let elems = List.rev elems in
       let+ types = Util.map_join (eval_ir_expr_type types_tab_stack) elems in
       StructLiteral (Struct (packed, types), Struct (packed, types), elems)
    | Parsetree.StructInit (pt, elems) ->
       let* silkt = Symtab.silktype_of_asttype types_tab_stack pt in
       let t = llvm_type_of_silktype silkt in
       let* resolved_t = resolve_alias types_tab_stack t in
       let check_elem acc elem =
         let+ exp = map_expr scope_map symtab_stack elem in
         exp :: acc
       in
       let+ elems = Util.flb check_elem [] elems in
       let elems = List.rev elems in
       StructLiteral (t, resolved_t, elems)
    | Parsetree.StructIndexAccess (exp, idx) ->
       let* expr = map_expr scope_map symtab_stack exp in
       let+ expr_type = eval_ir_expr_type types_tab_stack expr in
       StructAccess (expr_type, expr, idx)
    | Parsetree.StructMemberAccess (exp, name) ->
       let err = Error ("Error: Cannot access member '" ^ name
                        ^ "' of non-labeledstruct type in expression '"
                        ^ (Parsetree.show_expr expr) ^ "'")
       in
       let* expr = map_expr scope_map symtab_stack exp in
       let* expr_type = eval_ir_expr_type types_tab_stack expr in
       let rec find_member l e i = match l with
         | [] -> -1
         | (n, t) :: tl -> if n = e then i else find_member tl e (i + 1)
       in
       let* expr_type_resolved = resolve_alias types_tab_stack expr_type in
       let+ t = match expr_type_resolved with
         | StructLabeled (_, l) -> Ok l
         | OpaqueType _ -> Error "1unimplemented"
         | _ -> err
       in
       StructAccess (expr_type, expr, find_member t name 0)

    | Parsetree.ArrayElems elems ->
       let check_elem acc elem =
         let+ exp = map_expr scope_map symtab_stack elem in
         exp :: acc
       in
       let* elems = Util.flb check_elem [] elems in
       let+ elem_type = eval_ir_expr_type types_tab_stack (List.hd elems) in
       ArrayElems (Array (List.length elems, elem_type), List.rev elems)
    | Parsetree.ArrayInit (t, len) ->
       let+ t = Symtab.silktype_of_asttype types_tab_stack t in
       let t = llvm_type_of_silktype t in
       ArrayInit (Array (len, t))
    | Parsetree.Index (array_exp, idx_exp) ->
       let* array = map_expr scope_map symtab_stack array_exp in
       let* idx = map_expr scope_map symtab_stack idx_exp in
       let* array_type = eval_ir_expr_type types_tab_stack array in
       let* idx_type = eval_ir_expr_type types_tab_stack idx in
       let+ val_type = match array_type with
         | Array (_, t) -> Ok t
         | _ -> Error ("Error: Cannot index non-array type in expression '"
                       ^ (Parsetree.show_expr expr) ^ "'")
       in
       UnOp (Pointer val_type, Deref,
             GetElemPtr (array_type, Pointer array_type,
                         UnOp (array_type, AddressOf, array),
                         [(I 32, Literal (I 32, Int 0)); (idx_type, idx)]))
  in


  let rec map_stmt acc stmt =
    let blk_name i scopes =
      (String.concat "." (List.rev scopes)) ^ "." ^ (string_of_int i)
    in

    let map_blk blk acc =
      let (block_idx, scope_stack, scope_map, symtab_stack, ir_stmts) = acc in
      match Symtab.SymtabM.find (string_of_int block_idx) (List.hd symtab_stack) with
      | Symtab.Value (_, _, Some new_symtab) ->
         let+ stmts = (map_stmts blk ((string_of_int block_idx) :: scope_stack)
                         scope_map (new_symtab :: symtab_stack))
         in List.rev stmts
      | _ -> Error ("Error: Statement\n" ^ (Parsetree.show_stmt stmt)
                    ^ "\n is not a block.")
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
         Error ("Error: Expected value, found type '" ^ name ^ "' in statement\n"
                ^ (Parsetree.show_stmt stmt))
      | (scopes, _, Symtab.Value (_, type_, _)) ->
         let t = llvm_type_of_silktype type_ in
         let prefix = String.concat "." (List.rev scopes) in
         let resolved_name = "%\"" ^ prefix ^ "." ^ name ^ "\"" in
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
       let+ stmts = map_blk blk acc in
       (block_idx + 1, scope_stack, scope_map, symtab_stack,
        Block (blk_name block_idx scope_stack, stmts)
        :: ir_stmts)
    | Parsetree.IfElse (cond_expr, ifblock, elseblock) ->
       begin match (ifblock, elseblock) with
       | (Parsetree.Block ifstmts, Parsetree.Block elsestmts) ->
          let* cond_ir_exp = map_expr scope_map symtab_stack cond_expr in
          let* if_ir_stmts = map_blk ifstmts acc in
          let acc = (block_idx + 1, scope_stack,
                     scope_map, symtab_stack, ir_stmts) in
          let+ else_ir_stmts = map_blk elsestmts acc in
          let new_stmt = IfElse (blk_name block_idx scope_stack,
                                 blk_name (block_idx + 1) scope_stack,
                                 cond_ir_exp, if_ir_stmts,
                                 else_ir_stmts) in
          (block_idx + 2, scope_stack, scope_map, symtab_stack,
           new_stmt :: ir_stmts)
       | (Parsetree.Block _, _) ->
          Error ("Error: Else statement is not a block, found '"
                 ^ (Parsetree.show_stmt elseblock) ^ "' in statement\n"
                 ^ (Parsetree.show_stmt stmt))
       | _ -> Error ("Error: If statement is not a block, found '"
                     ^ (Parsetree.show_stmt ifblock) ^ "' in statement\n"
                     ^ (Parsetree.show_stmt stmt))
       end
    | Parsetree.While (cond_expr, whileblock) ->
       begin match whileblock with
       | Parsetree.Block whilestmts ->
          let* cond_ir_exp = map_expr scope_map symtab_stack cond_expr in
          let+ while_ir_stmts = map_blk whilestmts acc in
          let new_stmt = While (blk_name block_idx scope_stack,
                                cond_ir_exp, while_ir_stmts) in
          (block_idx + 1, scope_stack, scope_map,
           symtab_stack, new_stmt :: ir_stmts)
       | _ -> Error ("Error: While statement is not a block, found '"
                     ^ (Parsetree.show_stmt whileblock) ^ "' in statement\n"
                     ^ (Parsetree.show_stmt stmt))
       end
    | Parsetree.For (vd, cond_expr, inc_expr, forblock) ->
       begin match forblock with
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
          let+ for_ir_stmts = map_blk forstmts acc in
          let new_stmt = For (blk_name block_idx scope_stack,
                              (t, resolved_name, exp),
                              cond_ir_exp, inc_ir_exp,
                              for_ir_stmts) in
          (block_idx + 1, scope_stack, scope_map, symtab_stack,
           new_stmt :: ir_stmts)
       | _ -> Error ("Error: For statement is not a block, found '"
                     ^ (Parsetree.show_stmt forblock) ^ "' in statement\n"
                     ^ (Parsetree.show_stmt stmt))
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
         | Some (Ok ret_expr) ->
            let+ t = eval_ir_expr_type [symtab] ret_expr in
            Return (t, Some ret_expr)
         | None -> Ok (Return (Void, None))
       in
       let+ new_stmt = new_stmt_res in
       (block_idx, scope_stack, scope_map, symtab_stack, new_stmt :: ir_stmts)

  and map_stmts stmts scope_stack scope_map symtab_stack =
    let+ (_, _, _, _, ir_stmts) =
      Util.flb map_stmt (0, scope_stack, scope_map,
                         symtab_stack, []) stmts
    in
    ir_stmts
  in

  let map_top_decl acc td =
    let (scope_map, decls) = acc in
    match td with
    | Parsetree.TemplateFuncDecl _ | Parsetree.TemplateFuncFwdDecl _
      | Parsetree.TemplateTypeDef _ | Parsetree.TemplateTypeFwdDef _ -> Ok acc

    | Parsetree.TypeDef (name, _) ->
       begin match (Symtab.SymtabM.find name symtab) with
       | Symtab.Value _ -> Error ("Error: Expected type, found value '" ^ name ^ "'")
       | Symtab.Type t ->
          let t = llvm_type_of_silktype t in
          Ok (scope_map, (TypeDef (t, "%" ^ "\"" ^ name ^ "\"")) :: decls)
       end
    | Parsetree.TypeFwdDef _ -> Ok acc
    | Parsetree.ValDecl (pub, vd) ->
       let (name, expr) = match vd with
         | Parsetree.ValI (n, e)    -> (n, e)
         | Parsetree.Val  (n, _, e) -> (n, e)
         | Parsetree.VarI (n, e)    -> (n, e)
         | Parsetree.Var  (n, _, e) -> (n, e)
       in
       begin match (Symtab.SymtabM.find name symtab) with
       | Symtab.Type _ -> Error ("Error: Expected value, found type '" ^ name
                                 ^ "' in declaration\n" ^ (Parsetree.show_decl vd)
                                 ^ "\nin statement\n" ^ (Parsetree.show_top_decl td))
       | Symtab.Value (_, type_, _) ->
          let t = match expr with
            | Parsetree.Literal (Parsetree.LString s) ->
               Array ((String.length s) + 1, I 8)
            | _ -> llvm_type_of_silktype type_
          in
          let resolved_name = "@" ^ "\"" ^ name ^ "\"" in
          let+ l = resolve_literal expr in
          (ScopeM.add name resolved_name scope_map,
           (StaticDecl (t, pub, resolved_name, l)) :: decls)
       end
    | Parsetree.FuncDecl (pub, (name, args_, _, body)) ->
       let value_ = Symtab.SymtabM.find name symtab in
       begin match value_ with
       | Symtab.Type _ -> Error ("Error: Expected value, found type '" ^ name
                                 ^"' in declaration\n" ^ (Parsetree.show_top_decl td))
       | Symtab.Value (_, Symtab.Function (argtypes, rettype), inner_st) ->
          let rt = llvm_type_of_silktype rettype in
          let silktyped_args =
            List.map (fun ((n, _), t) -> (n, t)) (List.combine args_ argtypes)
          in
          let decl_of_arg = fun (n, t) ->
            let lt = llvm_type_of_silktype t in
            Decl (lt, "%\"" ^ name ^ "." ^ n ^ "\"", ParamIdentifier (lt, "%" ^ n))
          in
          let arg_decl_stmts = List.map decl_of_arg silktyped_args in
          let args =
            List.map
              (fun (n, t) -> (llvm_type_of_silktype t, "%" ^ n))
              silktyped_args
          in
          begin match body with
          | Parsetree.Block stmts ->
             let resolved_name = "@" ^ "\"" ^ name ^ "\"" in
             let scope_map_ = ScopeM.add name resolved_name scope_map in
             let scope_map =
               List.fold_left
                 (fun sm (n, _) ->
                   ScopeM.add n ("%\"" ^ name ^ "." ^ n ^ "\"") sm)
                 scope_map_ silktyped_args in
             let+ ir_stmts =
               map_stmts stmts [name] scope_map [Option.get inner_st]
             in
             let ir_stmts =
               if rt = Void then (Return (Void, None)) :: ir_stmts
               else ir_stmts
             in
             (scope_map_,
              (FuncDecl (rt, pub, resolved_name, args,
                         ir_stmts @ arg_decl_stmts)) :: decls)
          | _ -> Error ("Error: Function body\n" ^ (Parsetree.show_stmt body)
                        ^ "\nis not a block in declaration\n"
                        ^ (Parsetree.show_top_decl td))
          end
       | _ -> Error ("Error: Symbol '" ^ name ^ "' is not a function in declaration\n"
                     ^ (Parsetree.show_top_decl td))
       end
    | Parsetree.FuncFwdDecl (name, args_, _, extern) ->
       let value_ = Symtab.SymtabM.find name symtab in
       begin match value_ with
       | Symtab.Type _ -> Error ("Error: Expected value, found type '" ^ name
                                 ^ "' in declaration\n" ^ (Parsetree.show_top_decl td))
       | Symtab.Value (_, Symtab.Function (argtypes, rettype), _) ->
          let silktyped_args =
            List.map (fun ((n, _), t) -> (n, t)) (List.combine args_ argtypes)
          in
          let args =
            List.map
              (fun (n, t) -> (llvm_type_of_silktype t, "%" ^ n))
              silktyped_args
          in
          let resolved_name = "@" ^ "\"" ^ name ^ "\"" in
          let scope_map = ScopeM.add name resolved_name scope_map in
          let rt = llvm_type_of_silktype rettype in
          Ok (scope_map,
              (FuncFwdDecl
                 (rt, resolved_name, args, extern)) :: decls)
       | _ -> Error ("Error: Symbol '" ^ name ^ "' is not a function in declaration\n"
                     ^ (Parsetree.show_top_decl td))
       end
  in
  let+ (_, l) = Util.flb map_top_decl (ScopeM.empty, []) ast in
  List.rev l


let rec codegen_expr acc expr =
  let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
  match expr with
  | Identifier (t, name) ->
     begin match t with
     | Function (_, _) ->
        Ok (cont_label, brk_label, tmp_idx, tmp_value, insts, LNamed name)
     | _ ->
        let res = LTemporary tmp_idx in
        let new_inst = (res, Load (t, Pointer t, LNamed name)) in
        Ok (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)
     end
  | ParamIdentifier (t, name) ->
     Ok (cont_label, brk_label, tmp_idx, tmp_value, insts, LNamed name)
  | Literal (t, lit) ->
     begin match lit with
     | String_ s ->
        let array_t = Array ((String.length s) + 1, I 8) in
        let alloca_res = LTemporary tmp_idx in
        let alloca_inst = (alloca_res, Alloca array_t) in
        let store_inst =
          (NoValue, Store (array_t, LLiteral lit, Pointer array_t, alloca_res))
        in
        let bc_res = LTemporary (tmp_idx + 1) in
        let bc_inst =
          (bc_res, BitCast (Pointer array_t, alloca_res, Pointer (I 8)))
        in
        Ok (cont_label, brk_label, tmp_idx + 2, tmp_value,
            bc_inst :: store_inst :: alloca_inst :: insts, bc_res)
     | _ ->
        Ok (cont_label, brk_label, tmp_idx, tmp_value, insts, LLiteral lit)
     end
  | ArrayElems (t, exprs) ->
     let* val_type = match t with
       | Array (_, t) -> Ok t
       | _ -> Error "WTF"
     in
     let insert_value acc expr =
       let (acc, idx) = acc in
       let (_, _, _, _, _, array_value) = acc in
       let+ acc = codegen_expr acc expr in
       let (cont_label, brk_label, tmp_idx, tmp_value, insts, exp_value) = acc in
       let res = LTemporary tmp_idx in
       let new_inst =
         (res, InsertValue (t, array_value, val_type, exp_value, idx))
       in
       ((cont_label, brk_label, tmp_idx + 1, tmp_value,
         new_inst :: insts, res), idx + 1)
     in
     let acc = (cont_label, brk_label, tmp_idx, tmp_value, insts, LUndef) in
     let+ (acc, _) = Util.flb insert_value (acc, 0) exprs in acc
  | ArrayInit t -> Ok (cont_label, brk_label, tmp_idx, tmp_value, insts, LZeroInit)
  | StructLiteral (struct_t, struct_t_resolved, exprs) ->
     let* val_types = match discard_labels struct_t_resolved with
       | Struct (_, l) -> Ok l
       | _ -> Error "WTF"
     in
     let insert_value acc t expr =
       let* acc = acc in
       let (acc, idx) = acc in
       let (_, _, _, _, _, struct_value) = acc in
       let+ acc = codegen_expr acc expr in
       let (cont_label, brk_label, tmp_idx, tmp_value, insts, exp_value) = acc in
       let res = LTemporary tmp_idx in
       let new_inst =
         (res, InsertValue (struct_t, struct_value, t, exp_value, idx))
       in
       ((cont_label, brk_label, tmp_idx + 1, tmp_value,
         new_inst :: insts, res), idx + 1)
     in
     let acc = (cont_label, brk_label, tmp_idx, tmp_value, insts, LUndef) in
     let+ (acc, _) = List.fold_left2 insert_value (Ok (acc, 0)) val_types exprs in
     acc
  | StructAccess (t, expr, idx) ->
     let+ acc = codegen_expr acc expr in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, struct_value) = acc in
     let res = LTemporary tmp_idx in
     let new_inst = (res, ExtractValue (t, struct_value, idx)) in
     (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)
  | Assignment (t, name, rexpr) ->
     let+ (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) =
       codegen_expr acc rexpr
     in
     let new_inst = (NoValue, Store (t, last_result, Pointer t, LNamed name)) in
     (cont_label, brk_label, tmp_idx, tmp_value, new_inst :: insts, last_result)
  | StructAssign (struct_t, struct_expr, exprs) ->
     let* acc = codegen_expr acc struct_expr in
     let (cont_label, brk_label, tmp_idx, _, insts, struct_val) = acc in
     let add_assign acc expr =
       let (cont_label, brk_label, tmp_idx, _, insts, res) = acc in
       let acc =
         (cont_label, brk_label, tmp_idx, Some struct_val, insts, res)
       in
       codegen_expr acc expr
     in
     let+ acc = Util.flb add_assign acc exprs in
     let (cont_label, brk_label, tmp_idx, _, insts, struct_val) = acc in
     (cont_label, brk_label, tmp_idx, None, insts, struct_val)
  | Temporary t ->
     begin match tmp_value with
     | Some v -> Ok (cont_label, brk_label, tmp_idx, tmp_value, insts, v)
     | None -> Error "Error: No temporary value"
     end
  | Write (t, lexp, rexp) ->
     let* acc = codegen_expr acc lexp in
     let (_, _, _, _, _, ptr_val) = acc in
     let+ (cont_label, brk_label, tmp_idx, tmp_value, insts, r_val) =
       codegen_expr acc rexp
     in
     let new_inst = (NoValue, Store (t, r_val, Pointer t, ptr_val)) in
     (cont_label, brk_label, tmp_idx, tmp_value, new_inst :: insts, r_val)
  | FunctionCall (ft, fexp, ats, args) ->
     let* rt =  match ft with
       | Function (_, rt) -> Ok rt
       | _ -> Error "Error: Cannot call non-function type"
     in
     let* acc = codegen_expr acc fexp in
     let (_, _, _, _, _, f_value) = acc in
     let+ (args_rev, acc) =
       Util.flb
         (fun (args, acc) arg_expr ->
           let+ acc = codegen_expr acc arg_expr in
           let (_, _, _, _, _, arg_value) = acc in
           (arg_value :: args, acc))
         ([], acc) args
     in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
     let res = match rt with
       | Void -> NoValue
       | _ -> LTemporary tmp_idx
     in
     let new_inst = (res, Call (rt, f_value,
                                List.combine ats (List.rev args_rev))) in
     (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)
  | ItoF (at, exp, bt) ->
     let+ (cont_label, brk_label, tmp_idx, tmp_value, insts, v) =
       codegen_expr acc exp
     in
     let res = LTemporary tmp_idx in
     let new_inst = (res, ItoFP (at, v, bt)) in
     (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)
  | FtoI (at, exp, bt) ->
     let+ (cont_label, brk_label, tmp_idx, tmp_value, insts, v) =
       codegen_expr acc exp
     in
     let res = LTemporary tmp_idx in
     let new_inst = (res, FPtoI (at, v, bt)) in
     (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)
  | BitCast (at, exp, bt) ->
     let+ (cont_label, brk_label, tmp_idx, tmp_value, insts, v) =
       codegen_expr acc exp
     in
     let res = LTemporary tmp_idx in
     let new_inst = (res, BitCast (at, v, bt)) in
     (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)
  | PtoI (at, exp, bt) ->
     let+ (cont_label, brk_label, tmp_idx, tmp_value, insts, v) =
       codegen_expr acc exp
     in
     let res = LTemporary tmp_idx in
     let new_inst = (res, PtrToInt (at, v, bt)) in
     (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)
  | ItoP (at, exp, bt) ->
     let+ (cont_label, brk_label, tmp_idx, tmp_value, insts, v) =
       codegen_expr acc exp
     in
     let res = LTemporary tmp_idx in
     let new_inst = (res, IntToPtr (at, v, bt)) in
     (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)
  | Trunc (at, exp, bt) ->
     let+ (cont_label, brk_label, tmp_idx, tmp_value, insts, v) =
       codegen_expr acc exp
     in
     let res = LTemporary tmp_idx in
     let new_inst = (res, Trunc (at, v, bt)) in
     (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)
  | Ext (at, exp, bt) ->
     let+ (cont_label, brk_label, tmp_idx, tmp_value, insts, v) =
       codegen_expr acc exp
     in
     let res = LTemporary tmp_idx in
     let new_inst = (res, Ext (at, v, bt)) in
     (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)

  | GetElemPtr (val_type, ptr_type, ptr_expr, idxs) ->
     let* acc = codegen_expr acc ptr_expr in
     let (_, _, _, _, _, ptr_value) = acc in
     let add_idx acc (t, expr) =
       let (acc, idxs) = acc in
       let+ acc = codegen_expr acc expr in
       let (_, _, _, _, _, idx) = acc in
       (acc, (t, idx) :: idxs)
     in
     let+ (acc, idxs) = Util.flb add_idx (acc, []) idxs in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, idx_value) = acc in
     let res = LTemporary tmp_idx in
     let new_inst = (res,
                     GEP (val_type, ptr_type, ptr_value, List.rev idxs)) in
     (cont_label, brk_label, tmp_idx + 1, tmp_value, new_inst :: insts, res)

  | BinOp (t, op, l_expr, r_expr) ->
     let* acc = codegen_expr acc l_expr in
     let (_, _, _, _, _, l_value) = acc in
     let+ acc = codegen_expr acc r_expr in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, r_value) = acc in
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

     (cont_label, brk_label, tmp_idx + 1, tmp_value, (res, new_inst) :: insts, res)
  | UnOp (t, op, expr) ->
     match op with
     | AddressOf ->
        begin match expr with
        | Identifier (_, name) ->
           Ok (cont_label, brk_label, tmp_idx, tmp_value, insts, LNamed name)
        | UnOp (_, Deref, expr) -> codegen_expr acc expr
        | StructAccess (struct_type, struct_expr, idx) ->
           let rec resolve_addressof expr = match expr with
             | UnOp (_, Deref, expr) -> Ok expr
             | StructAccess (struct_type, struct_expr, idx) ->
                let+ struct_addr = resolve_addressof struct_expr in
                GetElemPtr (struct_type, Pointer struct_type, struct_addr,
                            [(I 32, Literal (I 32, Int 0));
                             (I 32, Literal (I 32, Int idx))])
             | Identifier _ -> Ok (UnOp (t, AddressOf, expr))
             | _ -> Error "Error: Cannot get address of temporary value"
           in
           let* expr = resolve_addressof expr in
           codegen_expr acc expr
        | _ -> Error "Error: Cannot get address of temporary value"
        end
     | _ ->
        let+ acc = codegen_expr acc expr in
        let (cont_label, brk_label, tmp_idx, tmp_value, insts, result) = acc in
        let res = LTemporary tmp_idx in
        let new_inst = match op with
          | UMinus ->
             begin match t with
             | F _ -> FNeg (t, result)
             | _ -> Sub (t, LLiteral (Int 0), result)
             end
          | Not -> Xor (t, result, LLiteral (Int (-1)))
          | Deref ->
             begin match t with
             | Pointer t -> Load (t, Pointer t, result)
             | _ -> NoInst
             end
          | _ -> NoInst
        in
        (cont_label, brk_label, tmp_idx + 1, tmp_value,
         (res, new_inst) :: insts, res)

let rec codegen_stmt acc stmt =
  let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
  match stmt with
  | Empty -> Ok acc
  | Decl (t, name, expr) ->
     let+ acc = codegen_expr acc expr in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
     let res = LNamed name in
     let alloca_inst = (LNamed name, Alloca t) in
     let store_inst = (NoValue, Store (t, last_result, Pointer t, res)) in
     (cont_label, brk_label, tmp_idx, tmp_value,
      store_inst :: alloca_inst :: insts, NoValue)
  | Expr expr -> codegen_expr acc expr
  | Block (name, stmts) ->
     let new_label = (NoValue, Label name) in
     let block_entry_inst = (NoValue, Branch name) in
     let acc = (cont_label, brk_label, tmp_idx, tmp_value,
                new_label :: block_entry_inst :: insts, last_result) in
     let+ acc = Util.flb codegen_stmt acc stmts in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
     let end_label = name ^ "_end" in
     let branch_inst = (NoValue, Branch end_label) in
     let end_inst = (NoValue, Label end_label) in
     (cont_label, brk_label, tmp_idx, tmp_value,
      end_inst :: branch_inst :: insts, last_result)
  | IfElse (if_label, else_label, cond_expr, if_stmts, else_stmts) ->
     let* acc = codegen_expr acc cond_expr in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
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
     let acc = (cont_label, brk_label, tmp_idx, tmp_value,
                if_label_inst :: branch_inst :: insts, NoValue) in
     let* acc = Util.flb codegen_stmt acc if_stmts in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
     let acc = (cont_label, brk_label, tmp_idx, tmp_value,
                else_label_inst :: if_end_branch_inst :: insts, NoValue) in
     let+ acc = Util.flb codegen_stmt acc else_stmts in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
     (cont_label, brk_label, tmp_idx, tmp_value,
      ifelse_end_label_inst :: else_end_branch_inst :: insts, NoValue)
  | While (while_label, cond_expr, while_stmts) ->
     let while_label_inst = (NoValue, Label while_label) in
     let while_end_label = while_label ^ "_end" in
     let while_end_label_inst = (NoValue, Label while_end_label) in
     let while_cond_label = while_label ^ "_cond" in
     let while_cond_label_inst = (NoValue, Label while_cond_label) in
     let branch_inst = (NoValue, Branch while_cond_label) in
     let acc = (Some while_cond_label, Some while_end_label, tmp_idx, tmp_value,
                while_label_inst :: branch_inst :: insts, NoValue) in
     let* acc = Util.flb codegen_stmt acc while_stmts in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, _) = acc in
     let branch_into_cond_inst = (NoValue, Branch while_cond_label) in
     let acc =
       (cont_label, brk_label, tmp_idx, tmp_value,
        while_cond_label_inst :: branch_into_cond_inst :: insts, NoValue)
     in
     let+ acc = codegen_expr acc cond_expr in
     let (_, _, tmp_idx, tmp_value, insts, last_result) = acc in
     let branch_cond_inst =
       (NoValue,
        BranchCond (last_result, while_label, while_end_label))
     in
     (None, None, tmp_idx, tmp_value,
      while_end_label_inst :: branch_cond_inst :: insts, NoValue)
  | For (for_label, decl, cond_expr, inc_expr, for_stmts) ->
     let* acc = codegen_stmt acc (Decl decl) in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
     let for_label_inst = (NoValue, Label for_label) in
     let for_end_label = for_label ^ "_end" in
     let for_end_label_inst = (NoValue, Label for_end_label) in
     let for_body_label = for_label ^ "_body" in
     let for_body_label_inst = (NoValue, Label for_body_label) in
     let for_inc_label = for_label ^ "_inc" in
     let for_inc_label_inst = (NoValue, Label for_inc_label) in
     let branch_into_for_inst = (NoValue, Branch for_label) in
     let acc =
       (Some for_inc_label, Some for_end_label, tmp_idx, tmp_value,
        for_label_inst :: branch_into_for_inst :: insts, NoValue)
     in
     let* acc = codegen_expr acc cond_expr in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
     let branch_cond_inst =
       (NoValue,
        BranchCond (last_result, for_body_label, for_end_label)) in
     let acc = (cont_label, brk_label, tmp_idx, tmp_value,
                for_body_label_inst :: branch_cond_inst :: insts, NoValue) in
     let* acc = Util.flb codegen_stmt acc for_stmts in
     let (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) = acc in
     let branch_into_inc_inst = (NoValue, Branch for_inc_label) in
     let acc =
       (cont_label, brk_label, tmp_idx, tmp_value,
        for_inc_label_inst :: branch_into_inc_inst :: insts,
        NoValue)
     in
     let+ acc = codegen_expr acc inc_expr in
     let (_, _, tmp_idx, tmp_value, insts, _) = acc in
     let branch_back_cond_inst = (NoValue, Branch for_label) in
     (None, None, tmp_idx, tmp_value,
      for_end_label_inst :: branch_back_cond_inst :: insts,
      NoValue)
  | Continue ->
     begin match cont_label with
     | None -> Error "Error: Invalid 'continue'"
     | Some label ->
        let branch_inst = (NoValue, Branch label) in
        Ok (cont_label, brk_label, tmp_idx, tmp_value,
            branch_inst :: insts, NoValue)
     end
  | Break ->
     begin match brk_label with
     | None -> Error "Error: Invalid 'break'"
     | Some label ->
        let branch_inst = (NoValue, Branch label) in
        Ok (cont_label, brk_label, tmp_idx, tmp_value, branch_inst :: insts, NoValue)
     end
  | Return (exp_type, exp_opt) ->
     begin match exp_opt with
     | None ->
        let ret_inst = (NoValue, Ret (Void, NoValue)) in
        Ok (cont_label, brk_label, tmp_idx, tmp_value, ret_inst :: insts, NoValue)
     | Some exp ->
        let+ (cont_label, brk_label, tmp_idx, tmp_value, insts, last_result) =
          codegen_expr acc exp
        in
        let ret_inst = (NoValue, Ret (exp_type, last_result)) in
        (cont_label, brk_label, tmp_idx, tmp_value, ret_inst :: insts, NoValue)
     end


let rec serialize_type t = match t with
  | Pointer t -> (serialize_type t) ^ "*"
  | Void -> "void"
  | Array (len, t) -> "[" ^ (string_of_int len)
                      ^ " x " ^ (serialize_type t) ^ "]"
  | Struct (packed, ts) ->
     let s = "{" ^ (String.concat ", " (List.map serialize_type ts)) ^ "}" in
     if packed then "<" ^ s ^ ">" else s
  | StructLabeled (packed, pairs) -> serialize_type @@ discard_labels t

  (* The Function type is actually a function pointer. *)
  | Function (ats, rt) ->
     (serialize_type rt) ^ "(" ^
       (String.concat ", " (List.map serialize_type ats)) ^
         ")*"

  | I i  | U i  -> "i" ^ (string_of_int i)
  | F 32 -> "float"
  | F 64 -> "double"
  | F _  -> "<Error>"
  | OpaqueType (name, _) -> name
  | Alias (t, name) -> name


let serialize_literal l = match l with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String_ s | GlobalString s ->
     let string_of_list l =
       let buf = Buffer.create 16 in
       List.iter (Buffer.add_char buf) l;
       Buffer.contents buf
     in
     let escape c = match (int_of_char c) with
       | i when i < 32 ->
          let s = Printf.sprintf "%.2X" i in
          ['\\'; s.[0]; s.[1]]
       | i -> [c]
     in
     let chars = List.init (String.length s) (String.get s) in
     let escaped = List.flatten @@ List.map escape chars in
     "c\"" ^ (string_of_list escaped) ^ "\\00\""

let rec serialize_value v = match v with
  | NoValue -> ""
  | LLiteral l -> serialize_literal l
  | LNamed name -> name
  | LTemporary i -> "%__tmp." ^ (string_of_int i)
  | LUndef -> "undef"
  | LZeroInit -> "zeroinitializer"

let is_unsigned t = match t with
  | U _ -> true
  | _ -> false

let is_float t = match t with
  | F _ -> true
  | _ -> false

let serialize_irt irt_roots =
  let rec serialize_inst = fun (value, inst) ->
    let inst_str i = match inst with
      | NoInst -> ""
      | Alloca t -> "alloca " ^ (serialize_type t)
      | Load (lt, t, v) -> "load " ^ (serialize_type lt) ^ ", "
                           ^ (serialize_type t) ^ " " ^ (serialize_value v)
      | GEP (vt, pt, pv, idxs) ->
         let serialize_idx (it, iv) =
           (serialize_type it) ^ " " ^ (serialize_value iv)
         in
         String.concat " " ["getelementptr"; (serialize_type vt) ^ ",";
                            serialize_type pt; (serialize_value pv) ^ ",";
                            String.concat ", " (List.map serialize_idx idxs)]
      | InsertValue (at, av, it, iv, idx) ->
         String.concat " " ["insertvalue"; serialize_type at;
                            (serialize_value av) ^ ",";
                            serialize_type it; (serialize_value iv) ^ ",";
                            string_of_int idx]
      | ExtractValue (t, v, i) ->
         String.concat " " ["extractvalue"; serialize_type t;
                            (serialize_value v) ^ ","; string_of_int i]
      | Store (t1, v1, t2, v2) -> String.concat " " ["store";
                                                     serialize_type t1;
                                                     (serialize_value v1) ^ ",";
                                                     serialize_type t2;
                                                     serialize_value v2]
      | Label s -> "\"" ^ s ^ "\":"
      | Branch l -> "br label %" ^ "\"" ^ l ^ "\""
      | BranchCond (v, ifl, elsel) -> "br i1 " ^ (serialize_value v) ^ ", label %\""
                                      ^ ifl ^ "\", label %\"" ^ elsel ^ "\""
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
      | PtrToInt (at, v, bt) ->
         String.concat " "
           ["ptrtoint";
            serialize_type at;
            serialize_value v;
            "to";
            serialize_type bt]
      | IntToPtr (at, v, bt) ->
         String.concat " "
           ["inttoptr";
            serialize_type at;
            serialize_value v;
            "to";
            serialize_type bt]
      | Trunc (at, v, bt) ->
         String.concat " "
           [if is_float bt then "fptrunc" else "trunc";
            serialize_type at;
            serialize_value v;
            "to";
            serialize_type bt]
      | Ext (at, v, bt) ->
         String.concat " "
           [if is_float at then "fpext" else
              if is_unsigned bt then "zext" else "sext";
            serialize_type at;
            serialize_value v;
            "to";
            serialize_type bt]
    in
    match value with
    | NoValue -> inst_str inst
    | v -> (serialize_value v) ^ " = " ^ (inst_str inst)
  in

  let rec serialize_irt str_insts root =
    match root with
    | StaticDecl (t, pub, name, l) ->
       let access_str = if pub then "global" else "private global" in
       begin match l with
       | GlobalString s ->
          let str_name = name ^ ".str" in
          let+ str_insts =
            serialize_irt str_insts @@ StaticDecl (t, false, str_name, String_ s)
          in
          let t1_str = serialize_type @@ Pointer t in
          let t2_str = serialize_type @@ Pointer (I 8) in
          let bc_str =
            String.concat " " ["bitcast"; "("; t1_str; str_name; "to"; t2_str; ")"]
          in
          let s =
            String.concat " " [name; "="; access_str; t2_str; bc_str]
          in
          s :: str_insts
       | _ ->
          let t_str = serialize_type t in
          let l_str = serialize_literal l in
          let s = String.concat " " [name; "="; access_str; t_str; l_str] in
          Ok (s :: str_insts)
       end
    | FuncDecl (rt, pub, funcname, args, body) ->
       let t_str = serialize_type rt in
       let args_str = String.concat ", "
                        (List.map
                           (fun (t, n) -> (serialize_type t) ^ " " ^ n)
                           args) in
       let access_str = if pub then "define " else "define private " in
       let ns = access_str ^ t_str ^ " " ^ funcname ^ "(" ^ args_str ^ ") {" in
       let+ (_, _, _, _, insts, _) =
         Util.flb
           codegen_stmt (None, None, 0, None, [], NoValue) (List.rev body)
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
    | TypeDef (t, name) ->
       let s = String.concat " " [name; "="; "type"; serialize_type t] in
       Ok (s :: str_insts)
  in
  let+ l = Util.flb serialize_irt [] irt_roots in
  String.concat "\n" (List.rev l)
