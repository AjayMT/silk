
(**
 * Type checking and symbol table construction.
 *)

module SymtabM = Map.Make(String)

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
              | TypeStub of string

let rec show_silktype st = match st with
  | I i -> "i" ^ (string_of_int i) | U i -> "u" ^ (string_of_int i)
  | F i -> "f" ^ (string_of_int i)
  | Bool -> "bool" | Void -> "void"
  | Pointer st -> "*" ^ (show_silktype st)
  | MutPointer st -> "mut*" ^ (show_silktype st)
  | Array (i, st) -> "[" ^ (string_of_int i) ^ "]" ^ (show_silktype st)
  | Struct (packed, sts) ->
     let prefix = if packed then "(:" else "(" in
     let suffix = if packed then ":)" else ")" in
     prefix ^ (String.concat "," @@ List.map show_silktype sts) ^ suffix
  | StructLabeled (packed, pairs) ->
     let prefix = if packed then "(:" else "(" in
     let suffix = if packed then ":)" else ")" in
     let serialize_pair (n, t) = n ^ " " ^ (show_silktype t) in
     prefix ^ (String.concat "," @@ List.map serialize_pair pairs) ^ suffix
  | Function (args, rt) ->
     "func (" ^ (String.concat ", " @@ List.map show_silktype args) ^ ") "
     ^ (show_silktype rt)
  | TypeAlias (s, _) -> s
  | TypeStub s -> s

(**
 * The symbol table is a map of names (string) to symbols (Type or Value).
 * The properties of the Value variant are:
 *     1: whether the value is immutable   (bool)
 *     2: the type of the value            (silktype)
 *     3: an optional 'child' symbol table (symbol SymtabM.t option)
 *
 * The third property is used to store the local symbol tables of functions
 * and blocks. This makes the symbol table of the entire program a tree;
 * the root only has top-level types and declarations, and each function/block
 * has a child symbol table containing its local symbols.
 *)

type symbol = Value of bool * silktype * symbol SymtabM.t option
            | Type of silktype


let ( let* ) x f = Result.bind x f
let ( let+ ) x f = Result.map f x

(**
 * find_symtab_stack finds a symbol with a given name in a 'stack' of symbol tables.
 * The 'stack' is a list of symbol tables starting with the innermost/lowest and
 * going to the outermost (top-level symbols) i.e the root.
 * The ordering of the stack means that this function will always find the closest
 * in scope i.e most local symbol with a given name.
 *)

let rec find_symtab_stack name ststack = match ststack with
  | [] -> None
  | (z :: zs) ->
     match SymtabM.find_opt name z with
     | Some v -> Some v
     | None -> find_symtab_stack name zs


(**
 * resolve_type_alias finds the base type of a type alias or newtype.
 * When resolving a forward-declared type (type stub) it searches the symbol table.
 *)

let rec resolve_type_alias symtab_stack t = match t with
  | TypeAlias (_, t) -> resolve_type_alias symtab_stack t
  | TypeStub name ->
     begin match find_symtab_stack name symtab_stack with
     | Some (Type t) -> resolve_type_alias symtab_stack t
     | _ -> Error ("Error: Failed to resolve type " ^ name)
     end
  | _ -> Ok t


(**
 * compare_types compares two types for structural equivalence, accounting for type
 * aliases and newtypes.
 *)

let rec compare_types symtab_stack a b =
  let compare_types = compare_types symtab_stack in
  match (a, b) with
  | (Function (aargtypes, arettype), Function (bargtypes, brettype)) ->
     let f b t1 t2 = if b then compare_types t1 t2 else b in
     List.length aargtypes = List.length bargtypes
     && (List.fold_left2 f true aargtypes bargtypes)
     && (compare_types arettype brettype)
  | (TypeStub a, TypeAlias (b, _)) -> a = b
  | (TypeAlias (a, _), TypeStub b) -> a = b
  | (TypeStub a, b) ->
     begin match find_symtab_stack a symtab_stack with
     | Some (Type t) -> compare_types t b
     | _ -> false
     end
  | (a, TypeStub b) ->
     begin match find_symtab_stack b symtab_stack with
     | Some (Type t) -> compare_types a t
     | _ -> false
     end
  | (TypeAlias (an, _), TypeAlias (bn, _)) ->
     if an = bn then true
     else
       begin match (resolve_type_alias symtab_stack a) with
       | Ok t -> compare_types t b
       | _ -> false
       end
  | (TypeAlias (_, a), b) ->
     (* aliases of structs are newtypes *)
     begin match (resolve_type_alias symtab_stack a) with
     | Ok Struct _ | Ok StructLabeled _ -> false
     | Ok a -> compare_types a b
     | _ -> false
     end
  | (a, TypeAlias (_, b)) ->
     begin match (resolve_type_alias symtab_stack b) with
     | Ok Struct _ | Ok StructLabeled _ -> false
     | Ok b -> compare_types a b
     | _ -> false
     end
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


(**
 * check_valid_cast checks whether one type can be cast to another.
 *)

let rec check_valid_cast symtab_stack cast_t expr_t =
  let check_valid_cast = check_valid_cast symtab_stack in
  let compare_types = compare_types symtab_stack in
  let err =
    Error ("Error: Invalid type cast from '" ^ (show_silktype expr_t)
           ^ "' to '" ^ (show_silktype cast_t) ^ "'")
  in
  match cast_t with
  | I _ | U _ | Bool ->
     begin match expr_t with
     | I _ | U _ | F _ | MutPointer _ | Bool -> Ok ()
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
  | Struct _ -> err
  | StructLabeled _ -> err
  | TypeAlias (_, a) -> check_valid_cast a expr_t
  | _ -> if compare_types cast_t expr_t then Ok ()
         else err

(** silktype_of_literal_type converts a literal type to a silktype *)
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


(**
 * silktype_of_asttype converts an AST type into a silk type.
 * It looks up aliases in the symbol table and ensures that there
 * are no uninstantiated templates.
 *)

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
     let+ types = Util.flb add_type [] types in
     Struct (packed, List.rev types)
  | Parsetree.StructLabeled (packed, members) ->
     let (names, types) = List.split members in
     let add_type acc t =
       let+ t = silktype_of_asttype symtab_stack t in
       t :: acc
     in
     let+ types = Util.flb add_type [] types in
     StructLabeled (packed, List.combine names (List.rev types))
  | Parsetree.Function (ats, rt) ->
     let define_arg acc a =
       let+ a = silktype_of_asttype symtab_stack a in
       a :: acc
     in
     let* args_rev = Util.flb define_arg [] ats in
     let args = List.rev args_rev in
     let+ rt = silktype_of_asttype symtab_stack rt in
     Function (args, rt)
  | Parsetree.TypeAlias name ->
     begin match find_symtab_stack name symtab_stack with
     | Some (Type t) -> Ok (TypeAlias (name, t))
     | Some (Value _) ->
        Error ("Error: '" ^ name ^ "' is not a type")
     | None -> Error ("Error: type '" ^ name ^ "' undefined")
     end
  | Parsetree.AliasTemplateInstance _ ->
     Error ("Error: Failed to instantiate template '" ^ (Parsetree.show_type t) ^ "'")
  | Parsetree.Template n -> Error ("Error: Failed to replace template '" ^ n ^ "'")


(**
 * eval_expr_type evaluates the type of an expression.
 * In doing so, it ensures that the expression is semantically valid and does
 * not violate typing or mutability rules.
 *)

let rec eval_expr_type symtab_stack expr =
  let compare_types = compare_types symtab_stack in
  let check_valid_cast = check_valid_cast symtab_stack in

  (* check_mutable checks if an expression is mutable *)
  let rec check_mutable exp = match exp with
    | Parsetree.Identifier name ->
       let v = find_symtab_stack name symtab_stack in
       begin match v with
       | Some (Value (false, _, _)) -> Ok true
       | Some (Value (true, _, _)) -> Ok false
       | _ -> Error ("Error: Expected value, found type '" ^ name
                     ^ "' in expression '" ^ (Parsetree.show_expr exp) ^ "'")
       end
    | Parsetree.UnOp (Parsetree.Deref, dexp) ->
       let* t = eval_expr_type symtab_stack dexp in
       begin match t with
       | MutPointer _ -> Ok true
       | Pointer _ -> Ok false
       | _ ->
          Error ("Error: Incorrect type for unary operation in expression '"
                 ^ (Parsetree.show_expr exp) ^ "'")
       end
    | Parsetree.Index (array, _) -> check_mutable array
    | Parsetree.StructIndexAccess (struct_expr, _)
      | Parsetree.StructMemberAccess (struct_expr, _) -> check_mutable struct_expr
    | _ ->
       Error ("Error: Cannot check mutability of temporary value in expression '"
              ^ (Parsetree.show_expr exp) ^ "'")
  in

  match expr with
  | Parsetree.TemplateInstance (name, types) ->
     Error ("Error: Failed to instantiate template in expression '" ^
              (Parsetree.show_expr expr) ^ "'")

  | Parsetree.Identifier name ->
     begin match find_symtab_stack name symtab_stack with
     | Some (Type _) ->
        Error ("Error: Expected value, found type '" ^ name
               ^ "' in expression '" ^ (Parsetree.show_expr expr) ^ "'")
     | Some (Value (_, t, _)) -> Ok t
     | None -> Error ("Error: Identifier '" ^ name ^ "' undefined in expression '"
                      ^ (Parsetree.show_expr expr) ^ "'")
     end
  | Parsetree.Literal l -> Ok (silktype_of_literal_type l)

  | Parsetree.Assignment (lval, e) ->
     let* exprtype = eval_expr_type symtab_stack e in

     (** check_lval checks the type of an lval and that it is mutable
       in an assignment expression. The use of this recursive function is
       necessitated by Silk's struct/group assignment semantics. *)
     let rec check_lval lval expected_type =
       let* lval_type = eval_expr_type symtab_stack lval in
       match lval with
       | Parsetree.StructLiteral (_, exprs) ->
          (* struct assignments have to be handled specially: each member
            is checked instead of the whole struct, since unlabeled
            struct literals can be assigned to labeled structs. *)
          let err =
            Error ("Error: Mismatched type of lval '"
                   ^ (Parsetree.show_expr lval)
                   ^ "', expected '" ^ (show_silktype expected_type)
                   ^ "' but found '" ^ (show_silktype lval_type)
                   ^ "' in struct assignment in expression '"
                   ^ (Parsetree.show_expr expr) ^ "'")
          in
          let add_lval acc lv et =
            let* acc = acc in
            let+ t = check_lval lv et in t
          in
          let* resolved_type = resolve_type_alias symtab_stack expected_type in
          let* valtypes = match resolved_type with
            | Struct (_, l) -> if List.length exprs <> List.length l then err
                               else Ok l
            | StructLabeled (_, l) ->
               if List.length exprs <> List.length l then err
               else Ok ((fun (_, a) -> a) @@ List.split l)
            | _ -> err
          in
          let+ lvals = List.fold_left2 add_lval (Ok expected_type) exprs valtypes in
          expected_type
       | _ ->
          (* Checking every other type of lvalue is a simple type comparison
             and mutability check. *)
          let* (err1, err2) = match lval with
            | Parsetree.Identifier (n) ->
               Ok ("assignment of '" ^ n ^ "'", "re-assign val '" ^ n ^ "'")
            | Parsetree.UnOp (Parsetree.Deref, _) ->
               Ok ("pointer assignment", "write immutable pointer")
            | Parsetree.Index (_, _) ->
               Ok ("array index assignment", "write immutable array")
            | Parsetree.StructIndexAccess (_, _)
              | Parsetree.StructMemberAccess (_, _) ->
               Ok ("struct member assignment", "write immutable struct")
            | _ -> Error ("Error: Invalid lvalue expression '"
                          ^ (Parsetree.show_expr lval) ^ "' in expression '"
                          ^ (Parsetree.show_expr expr) ^ "'")
          in
          let* mut = check_mutable lval in
          if mut then
            if compare_types lval_type expected_type then Ok expected_type
            else Error ("Error: Mismatched types in " ^ err1
                        ^ ", expected '" ^ (show_silktype expected_type)
                        ^ "' but found '" ^ (show_silktype lval_type)
                        ^ "' in expression '"
                        ^ (Parsetree.show_expr expr) ^ "'")
          else
            Error ("Error: Cannot " ^ err2 ^ " in expression '"
                   ^ (Parsetree.show_expr expr) ^ "'")
     in
     check_lval lval exprtype

  | Parsetree.TypeCast (t, exp) ->
     let* expr_t = eval_expr_type symtab_stack exp in
     let* cast_t = silktype_of_asttype symtab_stack t in
     begin match check_valid_cast cast_t expr_t with
     | Ok () -> Ok cast_t
     | Error e -> Error (e ^ " in expression '" ^ (Parsetree.show_expr expr) ^ "'")
     end

  | Parsetree.FunctionCall (f, args) ->
     (* function calls have to be treated specially because of the limitations
        of the parser, since (non-primitive-)type casts and struct initialization
        are parsed as function calls. *)

     (** match_args matches a list of expressions with a list of (argument) types.
        It allows mutable pointers to be promoted to immutable pointers *)
     let match_arg_types argtypes exprs =
       let match_types acc t exp =
         let* _ = acc in
         let check_arg_type et =
           let (a, b) = match (t, et) with
             | (Pointer a, MutPointer b) -> (a, b)
             | (a, b) -> (a, b) in
           if compare_types a b then Ok t
           else Error ("Error: Mismatched types of expression '"
                       ^ (Parsetree.show_expr exp)
                       ^ "', expected '" ^ (show_silktype et)
                       ^ "' but found '" ^ (show_silktype t)
                       ^ "' in function call in expression '"
                       ^ (Parsetree.show_expr expr) ^ "'")
         in
         Result.bind (eval_expr_type symtab_stack exp) check_arg_type
       in
       if List.length argtypes = List.length args then
         List.fold_left2 match_types (Ok Bool) argtypes exprs
       else Error ("Error: Incorrect number of arguments, expected "
                   ^ (string_of_int @@ List.length argtypes) ^ " but found "
                   ^ (string_of_int @@ List.length args)
                   ^ " in function call in expression '"
                   ^ (Parsetree.show_expr expr) ^ "'")
     in

     let check_function_type stype = match stype with
       | Function (argtypes, t) ->
          let+ _ = match_arg_types argtypes args in t
       | _ -> Error ("Error: Expression of type '"
                     ^ (show_silktype stype)
                     ^ "' is not a function in function call in expression '"
                     ^ (Parsetree.show_expr expr) ^ "'")
     in

     begin match (eval_expr_type symtab_stack f) with
     | Ok t -> check_function_type t
     | Error e ->
        (* if we fail to evaluate the expression as a function call (if 'f'
           is not a function expression), we try to evaluate it as a type
           cast or struct initialization. *)
        begin match f with
        | Parsetree.Identifier t ->
           let* silktype =
             silktype_of_asttype symtab_stack (Parsetree.TypeAlias t)
           in
           let* resolved_type = resolve_type_alias symtab_stack silktype in
           begin match resolved_type with
           | Struct (_, l) ->
              if List.length args <> List.length l
              then Error ("Error: Incorrect number of members, expected "
                          ^ (string_of_int @@ List.length l) ^ " but found "
                          ^ (string_of_int @@ List.length args)
                          ^ " in struct initialization in expression '"
                          ^ (Parsetree.show_expr expr) ^ "'")
              else eval_expr_type symtab_stack
                     (Parsetree.StructInit (Parsetree.TypeAlias t, args))
           | StructLabeled (_, l) ->
              if List.length args <> List.length l
              then Error ("Error: Incorrect number of members, expected "
                          ^ (string_of_int @@ List.length l) ^ " but found "
                          ^ (string_of_int @@ List.length args)
                          ^ " in struct initialization in expression '"
                          ^ (Parsetree.show_expr expr) ^ "'")
              else eval_expr_type symtab_stack
                     (Parsetree.StructInit (Parsetree.TypeAlias t, args))
           | _ ->
              if List.length args <> 1
              then Error ("Error: Invalid type cast in expression '"
                          ^ (Parsetree.show_expr expr) ^ "'")
              else eval_expr_type symtab_stack
                     (Parsetree.TypeCast (Parsetree.TypeAlias t, List.hd args))
           end
        | _ -> Error e
        end
     end

  | Parsetree.BinOp (a, op, b) ->
     let* a_type_unresolved = eval_expr_type symtab_stack a in
     let* a_type = resolve_type_alias symtab_stack a_type_unresolved in
     let* b_type_unresolved = eval_expr_type symtab_stack b in
     let* b_type = resolve_type_alias symtab_stack b_type_unresolved in
     let err =
       Error ("Error: Incorrect types '" ^ (show_silktype a_type_unresolved)
              ^ "', '" ^ (show_silktype b_type_unresolved)
              ^ "' for binary operation in expression '"
              ^ (Parsetree.show_expr expr) ^ "'")
     in
     begin match op with
     | Plus | Minus ->
        begin match (a_type, b_type) with
        | (I a, I b) | (U a, U b) | (F a, F b) -> if a = b then Ok a_type else err
        | (Pointer _, I _) | (Pointer _, U _)
          | (MutPointer _, I _) | (MutPointer _, U _) -> Ok a_type
        | _ -> err
        end
     | Times | Divide ->
        begin match (a_type, b_type) with
        | (I a, I b) | (U a, U b) | (F a, F b) -> if a = b then Ok a_type else err
        | _ -> err
        end
     | Modulus | BitAnd | BitOr | BitXor
       | RShift | LShift ->
        begin match (a_type, b_type) with
        | (I a, I b) | (U a, U b) -> if a = b then Ok a_type else err
        | _ -> err
        end
     | Equal | LessThan | GreaterThan ->
        begin match (a_type, b_type) with
        | (I a, I b) | (U a, U b) | (F a, F b) -> if a = b then Ok Bool else err
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
  | Parsetree.UnOp (op, uexpr) ->
     let* t_unresolved = eval_expr_type symtab_stack uexpr in
     let* t = resolve_type_alias symtab_stack t_unresolved in
     let err =
       Error ("Error: Incorrect type '"
              ^ (show_silktype t_unresolved) ^ "' for unary operation in expression '"
              ^ (Parsetree.show_expr expr) ^ "'")
     in
     begin match op with
     | Parsetree.UMinus ->
        begin match t with
        | I _ | U _ | F _ -> Ok t
        | _ -> err
        end
     | Parsetree.BitNot ->
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
        begin match uexpr with
        | Parsetree.Identifier name ->
           let v = find_symtab_stack name symtab_stack in
           begin match v with
           | Some (Value (false, _, _)) -> Ok (MutPointer t_unresolved)
           | Some (Value (true, _, _)) -> Ok (Pointer t_unresolved)
           | _ -> err
           end
        | Parsetree.UnOp (Parsetree.Deref, exp) -> eval_expr_type symtab_stack exp
        | Parsetree.Index (array, _) ->
           let+ mut = check_mutable array in
           if mut then MutPointer t_unresolved else Pointer t_unresolved
        | Parsetree.StructIndexAccess (exp, _)
          | Parsetree.StructMemberAccess (exp, _) ->
           let+ mut = check_mutable exp in
           if mut then MutPointer t_unresolved else Pointer t_unresolved
        | _ -> Error ("Error: Cannot get address of temporary value '"
                      ^ (Parsetree.show_expr uexpr) ^ "' in expression '"
                      ^ (Parsetree.show_expr expr) ^ "'")
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
     let+ types = Util.flb add_expr [] exprs in
     Struct (packed, List.rev types)
  | Parsetree.StructInit (t, exprs) ->
     let* t = silktype_of_asttype symtab_stack t in
     let* resolved_type = resolve_type_alias symtab_stack t in
     let* member_types = match resolved_type with
       | Struct (_, ts) ->
          if List.length ts = List.length exprs
          then Ok ts
          else Error ("Error: Incorrect number of members, expected "
                      ^ (string_of_int @@ List.length ts) ^ " but found "
                      ^ (string_of_int @@ List.length exprs)
                      ^ " in struct initialization in expression '"
                      ^ (Parsetree.show_expr expr) ^ "'")
       | StructLabeled (_, pairs) ->
          if List.length pairs = List.length exprs
          then Ok ((fun (_, s) -> s) @@ List.split pairs)
          else Error ("Error: Incorrect number of members, expected "
                      ^ (string_of_int @@ List.length pairs) ^ " but found "
                      ^ (string_of_int @@ List.length exprs)
                      ^ " in struct initialization in expression '"
                      ^ (Parsetree.show_expr expr) ^ "'")
       | _ -> Error ("Error: Cannot initialize non-struct type '"
                     ^ (show_silktype resolved_type) ^ "' in expression '"
                     ^ (Parsetree.show_expr expr) ^ "'")
     in
     let add_member acc (e, t) =
       let* et = eval_expr_type symtab_stack e in
       if compare_types et t then Ok ()
       else Error ("Error: Mismatched types '"
                   ^ (show_silktype et) ^ "' and '" ^ (show_silktype t)
                   ^ "' in struct initialization in expression '"
                   ^ (Parsetree.show_expr expr) ^ "'")
     in
     let+ () = Util.flb add_member () (List.combine exprs member_types) in
     t

  | Parsetree.ArrayElems [] ->
     Error ("Error: Empty array initializer in expression '"
            ^ (Parsetree.show_expr expr) ^ "'")
  | Parsetree.ArrayElems (head :: tail) ->
     let check_elem acc elem =
       let* elem_type = eval_expr_type symtab_stack elem in
       if compare_types acc elem_type then Ok elem_type
       else Error ("Error: Mismatched types '"
                   ^ (show_silktype acc) ^ "' and '" ^ (show_silktype elem_type)
                   ^ "' in array initializer in expression '"
                   ^ (Parsetree.show_expr expr) ^ "'")
     in
     let* head_type = eval_expr_type symtab_stack head in
     let+ elem_type = Util.flb check_elem head_type tail in
     Array (List.length (head :: tail), elem_type)
  | Parsetree.ArrayInit (t, len) ->
     let* t = silktype_of_asttype symtab_stack t in
     if len < 0 then Error ("Error: Negative array length in expression '"
                            ^ (Parsetree.show_expr expr) ^ "'")
     else Ok (Array (len, t))

  | Parsetree.Index (array, idx) ->
     let* array_type = eval_expr_type symtab_stack array in
     let* idx_type = eval_expr_type symtab_stack idx in
     let* resolved_type = resolve_type_alias symtab_stack idx_type in
     begin match resolved_type with
     | I _ | U _ ->
        let* resolved_type = resolve_type_alias symtab_stack array_type in
        begin match resolved_type with
        | Array (_, elem_type) -> Ok elem_type
        | _ -> Error ("Error: Cannot index non-array type '"
                      ^ (show_silktype resolved_type) ^ "' in expression '"
                      ^ (Parsetree.show_expr expr) ^ "'")
        end
     | _ -> Error ("Error: Array index must be integer type, found '"
                   ^ (show_silktype resolved_type) ^ "' in expression '"
                   ^ (Parsetree.show_expr expr) ^ "'")
     end
  | Parsetree.StructMemberAccess (exp, name) ->
     let* t = eval_expr_type symtab_stack exp in
     let* resolved_type = resolve_type_alias symtab_stack t in
     let* members = match resolved_type with
       | StructLabeled (_, l) -> Ok l
       | _ -> Error ("Error: Cannot access member '" ^ name
                     ^ "' of non-labeledstruct type '" ^ (show_silktype t)
                     ^ "' in expression '" ^ (Parsetree.show_expr expr) ^ "'")
     in
     begin match List.assoc_opt name members with
     | Some t -> Ok t
     | None -> Error ("Error: Struct of type '" ^ (show_silktype t)
                      ^ "' has no member named '" ^ name ^ "' in expression '"
                      ^ (Parsetree.show_expr expr) ^ "'")
     end
  | Parsetree.StructIndexAccess (exp, idx) ->
     let* t = eval_expr_type symtab_stack exp in
     let* resolved_type = resolve_type_alias symtab_stack t in
     let* members = match resolved_type with
       | Struct (_, l) -> Ok l
       | _ -> Error ("Error: Cannot access member " ^ (string_of_int idx)
                     ^ " of non-labeledstruct type '" ^ (show_silktype t)
                     ^ "' in expression '" ^ (Parsetree.show_expr expr) ^ "'")
     in
     match List.nth_opt members idx with
     | Some t -> Ok t
     | None -> Error ("Error: Struct of type '" ^ (show_silktype t)
                      ^ "' has no member at index " ^ (string_of_int idx)
                      ^ " in expression '" ^ (Parsetree.show_expr expr) ^ "'")


(**
 * trav_valdecl traverses a top-level value declaration (val or var) in the AST
 * and adds the appropriate entries to the symbol table.
 *)

let trav_valdecl symtab symtab_stack vd =
  let check_inferred_type mut ident expr =
    match SymtabM.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol '" ^ ident
                       ^ "' already defined in declaration '"
                       ^ (Parsetree.show_decl vd) ^ "'")
    | None ->
       let+ stype = eval_expr_type (symtab :: symtab_stack) expr in
       SymtabM.add ident (Value (mut, stype, None)) symtab
  in

  let check_declared_type mut ident asttype expr =
    match SymtabM.find_opt ident symtab with
    | Some _ -> Error ("Error: Symbol '" ^ ident
                       ^ "' already defined in declaration '"
                       ^ (Parsetree.show_decl vd) ^ "'")
    | None ->
       let* lstype = silktype_of_asttype (symtab :: symtab_stack) asttype in
       let* rstype = eval_expr_type (symtab :: symtab_stack) expr in
       if compare_types (symtab :: symtab_stack) lstype rstype then
         Ok (SymtabM.add ident (Value (mut, lstype, None)) symtab)
       else
         Error ("Error: Mismatched types '"
                ^ (show_silktype lstype) ^ "' and '" ^ (show_silktype rstype)
                ^ "' in declaration '" ^ (Parsetree.show_decl vd) ^ "'")
  in

  match vd with
  | Parsetree.ValI (ident, expr) -> check_inferred_type true ident expr
  | Parsetree.Val (ident, asttype, expr) ->
     check_declared_type true ident asttype expr
  | Parsetree.VarI (ident, expr) -> check_inferred_type false ident expr
  | Parsetree.Var (ident, asttype, expr) ->
     check_declared_type false ident asttype expr


(**
 * construct_block_symtab constructs the local symbol table of a block.
 *
 * Every block within a function/block is assigned an index: the first block
 * is 0, the second block is 1, and so on. Each block's local symbol table is
 * stored in the parent's symbol table using the block's index as the key.
 *
 * For example, the following function:
 *     func main() void {
 *         var a = 1; val baz = "asdf";
 *         { var b = 2; var c = 3; }
 *         { var d = 3; var g = 17; }
 *     }
 * Would have a symbol table with the following entries:
 *     a:   Value (false, i32, None)
 *     baz: Value (true, *i8, None)
 *     0:   Value (true, void, Some <symbol table of block 0>)
 *     1:   Value (true, void, Some <symbol table of block 1>)
 *)

let rec construct_block_symtab base_symtab symtab_stack rettype stmts =
  (** addblk adds a block symbol table (new_base) to the parent
     symbol table (symtab) *)
  let addblk block_number symtab new_base blk =
    let new_symtab st =
      SymtabM.add (string_of_int block_number)
        (Value (true, Void, Some st))
        symtab
    in
    let+ st =
      construct_block_symtab
        new_base (symtab :: symtab_stack) rettype blk
    in
    (block_number + 1, new_symtab st)
  in

  (** trav_stmt traverses a statement in the current block, adding entries
     to the symbol table as necessary *)
  let trav_stmt acc stmt =
    let (block_number, symtab) = acc in
    match stmt with
    | Parsetree.Empty -> Ok (block_number, symtab)
    | Parsetree.Decl vd ->
       let+ s = trav_valdecl symtab symtab_stack vd in
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
             | _ -> Error ("Error: Else statement is not a block, found '"
                           ^ (Parsetree.show_stmt elsestmt) ^ "' in statement\n"
                           ^ (Parsetree.show_stmt stmt))
             end
          | _ ->
             Error ("Error: Expected boolean expression in 'if' statement, found '"
                    ^ (Parsetree.show_expr exp) ^ "' in statement\n"
                    ^ (Parsetree.show_stmt stmt))
          end
       | _ -> Error ("Error: If statement is not a block, found '"
                     ^ (Parsetree.show_stmt ifstmt) ^ "' in statement\n"
                     ^ (Parsetree.show_stmt stmt))
       end
    | Parsetree.While (exp, whilestmt) ->
       begin match whilestmt with
       | Parsetree.Block blk ->
          let* expr_t = eval_expr_type (symtab :: symtab_stack) exp in
          begin match expr_t with
          | Bool -> addblk block_number symtab SymtabM.empty blk
          | _ ->
             Error ("Error: Expected boolean expression in 'while' statement, found '"
                    ^ (Parsetree.show_expr exp) ^ "' in statement\n"
                    ^ (Parsetree.show_stmt stmt))
          end
       | _ -> Error ("Error: While statement is not a block, found '"
                     ^ (Parsetree.show_stmt whilestmt) ^ "' in statement\n"
                     ^ (Parsetree.show_stmt stmt))
       end
    | Parsetree.For (vd, condexp, incexp, forblk) ->
       begin match forblk with
       | Parsetree.Block blk ->
          let* local_symtab =
            trav_valdecl SymtabM.empty (symtab :: symtab_stack) vd
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
             Error ("Error: Expected boolean expression in 'for' statement, found '"
                    ^ (Parsetree.show_expr condexp) ^ "' in statement\n"
                    ^ (Parsetree.show_stmt stmt))
          end
       | _ -> Error ("Error: For statement is not a block, found '"
                     ^ (Parsetree.show_stmt forblk) ^ "' in statement\n"
                     ^ (Parsetree.show_stmt stmt))
       end

    | Parsetree.Return exo ->
       begin match exo with
       | Some exp ->
          let* ext = eval_expr_type (symtab :: symtab_stack) exp in
          if compare_types symtab_stack rettype ext then
            Ok (block_number, symtab)
          else
            Error ("Error: Incorrect return type, expected '"
                   ^ (show_silktype rettype) ^ "' but found '" ^ (show_silktype ext)
                   ^ "' in expression '" ^ (Parsetree.show_expr exp)
                   ^ "' in statement\n" ^ (Parsetree.show_stmt stmt))
       | None ->
          if compare_types symtab_stack rettype Void then
            Ok (block_number, symtab)
          else
            Error ("Error: Incorrect return type, expected '"
                   ^ (show_silktype rettype) ^ "' but found '" ^ (show_silktype Void)
                   ^ "' in statement\n" ^ (Parsetree.show_stmt stmt))
       end
    | Parsetree.Continue | Parsetree.Break -> Ok (block_number, symtab)
  in
  let+ (_, s) = Util.flb trav_stmt (0, base_symtab) stmts in s


(**
 * construct_symtab constructs the symbol table of the entire program starting from
 * the top-level declarations.
 *)

let construct_symtab ast =
  (** trav_funcdecl traverses a function declaration and adds it (along with its
     local symbol table) to the symbol table *)
  let trav_funcdecl symtab (ident, arglist, ret_asttype) =
    (* We have to check if this function has already been forward-declared,
       and whether it matches the type of previous declaration if so. *)
    let fwd_decl_value = SymtabM.find_opt ident symtab in
    match fwd_decl_value with
    | (Some (Value (true, Function (_, _), None))) | None ->
       (* define_arg adds the function's arguments to its local symbol table *)
       let define_arg acc argtuple =
         let (new_symtab, argtypes) = acc in
         let (name, asttype) = argtuple in
         let* argtype = silktype_of_asttype [symtab] asttype in
         begin match SymtabM.find_opt name new_symtab with
         | Some _ -> Error ("Error: Duplicate argument '" ^ name
                            ^ "' in declaration of '" ^ ident ^ "'")
         | None -> Ok (SymtabM.add name (Value (true, argtype, None)) new_symtab,
                       argtype :: argtypes)
         end
       in
       let* (new_symtab, argtypes_r) =
         Util.flb define_arg (SymtabM.empty, []) arglist
       in
       let argtypes = List.rev argtypes_r in
       let* rettype = silktype_of_asttype [symtab] ret_asttype in
       let func_t = Function (argtypes, rettype) in

       (* Now we compare types of the previous declaration and the current one. *)
       begin match fwd_decl_value with
       | Some (Value (true, fwd_decl_t, None)) ->
          if compare_types [symtab] fwd_decl_t func_t then
            Ok (new_symtab, func_t)
          else Error ("Error: Types of '" ^ ident ^ "' do not match, found '"
                      ^ (show_silktype fwd_decl_t) ^ "' and '"
                      ^ (show_silktype func_t) ^ "'")
       | None -> Ok (new_symtab, func_t)
       | _ -> Error ("Error: Symbol '" ^ ident ^ "' already defined")
       end

    | Some _ -> Error ("Error: Symbol '" ^ ident ^ "' already defined")
  in

  (** trav_ast traverses a single top-level declaration and adds entries to the
     symbol table *)
  let trav_ast symtab decl = match decl with
    | Parsetree.TypeDef (ident, basetype) ->
       begin match SymtabM.find_opt ident symtab with
       | None | Some (Type (TypeStub _)) ->
          let+ t = silktype_of_asttype [symtab] basetype in
          SymtabM.add ident (Type t) symtab
       | Some _ -> Error ("Error: Symbol '" ^ ident ^ "' in declaration\n"
                          ^ (Parsetree.show_top_decl decl) ^ "\nalready defined")
       end
    | Parsetree.TypeFwdDef ident ->
       begin match SymtabM.find_opt ident symtab with
       | None -> Ok (SymtabM.add ident (Type (TypeStub ident)) symtab)
       | Some _ -> Error ("Error: Symbol '" ^ ident ^ "' in declaration\n"
                          ^ (Parsetree.show_top_decl decl) ^ "\nalready defined")
       end
    | Parsetree.ValDecl (_, vd) -> trav_valdecl symtab [] vd
    | Parsetree.FuncDecl (_, (ident, arglist, ret_asttype, body)) ->
       let* (new_symtab, ft) =
         trav_funcdecl symtab (ident, arglist, ret_asttype)
       in
       begin match body with
       | Parsetree.Block blk ->
          let nst = SymtabM.add ident (Value (true, ft, None)) symtab in
          let rt = match ft with
            | Function (_, rt) -> rt
            | _ -> Void
          in
          let+ st = construct_block_symtab new_symtab [nst] rt blk in
          SymtabM.add ident (Value (true, ft, Some st)) symtab
       | _ -> Error ("Error: Function body of '" ^ ident ^ "' is not a block, found\n"
                     ^ (Parsetree.show_stmt body))
       end
    | Parsetree.FuncFwdDecl (ident, arglist, ret_asttype, _) ->
       let+ (_, ft) = trav_funcdecl symtab (ident, arglist, ret_asttype) in
       SymtabM.add ident (Value (true, ft, None)) symtab

    | _ -> Ok symtab
  in
  Util.flb trav_ast SymtabM.empty ast
