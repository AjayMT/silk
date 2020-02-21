
(*
 * Template instantiation.
 *)

open Parsetree

module TemplateM = Map.Make(String)

let ( let* ) x f = Result.bind x f
let ( let+ ) x f = Result.map f x

let rec serialize_type t = match t with
  | I8  -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | U8  -> "u8"
  | U16 -> "u16"
  | U32 -> "u32"
  | U64 -> "u64"
  | F32 -> "f32"
  | F64 -> "f64"
  | Void -> "void"
  | Bool -> "bool"
  | TypeAlias name -> name
  | Template name -> name
  | Function (ats, rt) ->
     "func(" ^
       (String.concat "," @@ List.map serialize_type ats)
       ^ ")" ^ (serialize_type rt)
  | Pointer t -> "*" ^ (serialize_type t)
  | MutPointer t -> "mut*" ^ (serialize_type t)
  | Array (i, t) -> "[" ^ (string_of_int i) ^ "]" ^ (serialize_type t)
  | Struct (packed, ts) ->
     let prefix = if packed then "(:" else "(" in
     let suffix = if packed then ":)" else ")" in
     prefix ^ (String.concat "," @@ List.map serialize_type ts) ^ suffix
  | StructLabeled (packed, pairs) ->
     let prefix = if packed then "(:" else "(" in
     let suffix = if packed then ":)" else ")" in
     let serialize_pair (n, t) = n ^ "|" ^ (serialize_type t) in
     prefix ^ (String.concat "," @@ List.map serialize_pair pairs) ^ suffix
  | AliasTemplateInstance (name, ts) ->
     name ^ "<" ^ (String.concat "," @@ List.map serialize_type ts) ^ ">"

let serialize_instance name types =
  serialize_type @@ AliasTemplateInstance (name, types)

let add_object f (decls, os) o =
  let+ (decls, o) = f decls o in
  decls, o :: os

let rec map_type tmap t =
  match t with
  | Template name ->
     begin match TemplateM.find_opt name tmap with
     | Some t -> Ok t
     | None -> Error ("Error: Undefined template " ^ name)
     end
  | Function (ts, rt) ->
     let* rt = map_type tmap rt in
     let+ args = Util.map_join (map_type tmap) ts in
     Function (args, rt)
  | Pointer t -> let+ t = map_type tmap t in Pointer t
  | MutPointer t -> let+ t = map_type tmap t in MutPointer t
  | Array (i, t) -> let+ t = map_type tmap t in Array (i, t)
  | StructLabeled (packed, pairs) ->
     let (names, types) = List.split pairs in
     let+ mts = Util.map_join (map_type tmap) types in
     let pairs = List.combine names mts in
     StructLabeled (packed, pairs)
  | Struct (packed, ts) ->
     let+ mts = Util.map_join (map_type tmap) ts in
     Struct (packed, mts)
  | AliasTemplateInstance (name, ts) ->
     let+ ts = Util.map_join (map_type tmap) ts in
     AliasTemplateInstance (name, ts)
  | I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | F32 | F64
    | Void | Bool | TypeAlias _ -> Ok t

let rec map_expr tmap e = match e with
  | Identifier _ | Literal _ -> Ok e
  | Assignment (l, r) ->
     let* l = map_expr tmap l in
     let+ r = map_expr tmap r in
     Assignment (l, r)
  | TemplateInstance (name, ts) ->
     let+ ts = Util.map_join (map_type tmap) ts in
     TemplateInstance (name, ts)
  | StructLiteral (packed, es) ->
     let+ es = Util.map_join (map_expr tmap) es in
     StructLiteral (packed, es)
  | StructInit (t, es) ->
     let* t = map_type tmap t in
     let+ es = Util.map_join (map_expr tmap) es in
     StructInit (t, es)
  | ArrayElems es -> let+ es = Util.map_join (map_expr tmap) es in ArrayElems (es)
  | ArrayInit (t, i) -> let+ t = map_type tmap t in ArrayInit (t, i)
  | FunctionCall (fe, fes) ->
     let* fe = map_expr tmap fe in
     let+ fes = Util.map_join (map_expr tmap) fes in
     FunctionCall (fe, fes)
  | TypeCast (t, e) ->
     let* t = map_type tmap t in
     let+ e = map_expr tmap e in
     TypeCast (t, e)
  | BinOp (l, o, r) ->
     let* l = map_expr tmap l in
     let+ r = map_expr tmap r in
     BinOp (l, o, r)
  | UnOp (o, e) -> let+ e = map_expr tmap e in UnOp (o, e)
  | Index (a, b) ->
     let* a = map_expr tmap a in
     let+ b = map_expr tmap b in
     Index (a, b)
  | StructMemberAccess (e, s) ->
     let+ e = map_expr tmap e in StructMemberAccess (e, s)
  | StructIndexAccess (e, i) ->
     let+ e = map_expr tmap e in StructIndexAccess (e, i)

let rec map_stmt tmap s =
  let map_vd tmap vd = match vd with
    | ValI (n, e) -> let+ e = map_expr tmap e in ValI (n, e)
    | VarI (n, e) -> let+ e = map_expr tmap e in ValI (n, e)
    | Val (n, t, e) ->
       let* t = map_type tmap t in
       let+ e = map_expr tmap e in
       Val (n, t, e)
    | Var (n, t, e) ->
       let* t = map_type tmap t in
       let+ e = map_expr tmap e in
       Var (n, t, e)
  in
  match s with
  | Empty | Continue | Break | Return None -> Ok s
  | Decl vd -> let+ vd = map_vd tmap vd in Decl vd
  | Expr e -> let+ e = map_expr tmap e in Expr e
  | Block ss ->
     let+ ss = Util.map_join (map_stmt tmap) ss in
     Block ss
  | IfElse (e, s1, s2) ->
     let* e = map_expr tmap e in
     let* s1 = map_stmt tmap s1 in
     let+ s2 = map_stmt tmap s2 in
     IfElse (e, s1, s2)
  | While (e, s) ->
     let* e = map_expr tmap e in
     let+ s = map_stmt tmap s in
     While (e, s)
  | For (vd, e1, e2, s) ->
     let* vd = map_vd tmap vd in
     let* e1 = map_expr tmap e1 in
     let* e2 = map_expr tmap e2 in
     let+ s = map_stmt tmap s in
     For (vd, e1, e2, s)
  | Return (Some e) ->
     let+ e = map_expr tmap e in
     Return (Some e)

let rec create_decl name types decls =
  let alias_name = serialize_instance name types in
  let tmap templates =
    let add_template tmap (name, type_) = TemplateM.add name type_ tmap
    in
    List.fold_left add_template TemplateM.empty @@
      List.combine templates types
  in
  match List.assoc_opt alias_name decls with
  | Some _ -> Ok decls
  | None ->
     match Util.assoc2 name decls with
     | (Some (TemplateTypeDef (templates, (_, type_))),
        b) ->
        let decls = match b with
          | Some (TemplateTypeFwdDef (_, name)) ->
             (alias_name, TypeFwdDef alias_name) :: decls
          | _ -> decls
        in
        let* t = map_type (tmap templates) type_ in
        let decl = TypeDef (alias_name, t) in
        trav_top_decl decls decl
     | (Some (TemplateFuncDecl (pub, (templates, (_, args, rt, body)))), b) ->
        let* decls = match b with
          | Some (TemplateFuncFwdDecl (templates, (name, args, rt, ext))) ->
             let* rt = map_type (tmap templates) rt in
             let (argnames, argtypes) = List.split args in
             let+ argtypes = Util.map_join (map_type (tmap templates)) argtypes in
             let args = List.combine argnames argtypes in
             (alias_name, FuncFwdDecl (alias_name, args, rt, ext)) :: decls
          | _ -> Ok decls
        in
        let* body = map_stmt (tmap templates) body in
        let* rt = map_type (tmap templates) rt in
        let (argnames, argtypes) = List.split args in
        let* argtypes = Util.map_join (map_type (tmap templates)) argtypes in
        let args = List.combine argnames argtypes in
        let decl = FuncDecl (pub, (alias_name, args, rt, body)) in
        trav_top_decl decls decl
     | _ -> Error ("Error: Failed to create " ^ alias_name)

and trav_type decls t = match t with
  | I8 | I16 | I32 | I64
    | U8 | U16 | U32 | U64
    | F32 | F64 | Void | Bool | TypeAlias _ | Template _ -> Ok (decls, t)
  | Function (ts, rt) ->
     let* (decls, ts) = Util.flb (add_object trav_type) (decls, []) ts in
     let+ (decls, rt) = trav_type decls rt in
     decls, Function (List.rev ts, rt)
  | Pointer t ->
     let+ (decls, t) = trav_type decls t in
     decls, Pointer t
  | MutPointer t ->
     let+ (decls, t) = trav_type decls t in
     decls, MutPointer t
  | Array (i, t) ->
     let+ (decls, t) = trav_type decls t in
     decls, Array (i, t)
  | StructLabeled (packed, members) ->
     let (names, ts) = List.split members in
     let+ (decls, ts) = Util.flb (add_object trav_type) (decls, []) ts in
     let members = List.combine names @@ List.rev ts in
     decls, StructLabeled (packed, members)
  | Struct (packed, ts) ->
     let+ (decls, ts) = Util.flb (add_object trav_type) (decls, []) ts in
     decls, Struct (packed, List.rev ts)

  | AliasTemplateInstance (name, types) ->
     let+ decls = create_decl name types decls in
     decls, TypeAlias (serialize_instance name types)

and trav_expr decls e = match e with
  | Identifier _ | Literal _ -> Ok (decls, e)
  | Assignment (l, r) ->
     let* (decls, r) = trav_expr decls r in
     let+ (decls, l) = trav_expr decls l in
     decls, Assignment (l, r)
  | StructLiteral (packed, es) ->
     let+ (decls, es) = Util.flb (add_object trav_expr) (decls, []) es in
     decls, StructLiteral (packed, List.rev es)
  | StructInit (t, es) ->
     let* (decls, t) = trav_type decls t in
     let+ (decls, es) = Util.flb (add_object trav_expr) (decls, []) es in
     decls, StructInit (t, List.rev es)
  | ArrayElems es ->
     let+ (decls, es) = Util.flb (add_object trav_expr) (decls, []) es in
     decls, ArrayElems (List.rev es)
  | ArrayInit (t, i) ->
     let+ (decls, t) = trav_type decls t in
     decls, ArrayInit (t, i)
  | FunctionCall (e, es) ->
     let* (decls, e) = trav_expr decls e in
     let+ (decls, es) = Util.flb (add_object trav_expr) (decls, []) es in
     decls, FunctionCall (e, List.rev es)
  | TypeCast (t, e) ->
     let* (decls, t) = trav_type decls t in
     let+ (decls, e) = trav_expr decls e in
     decls, TypeCast (t, e)
  | BinOp (l, o, r) ->
     let* (decls, l) = trav_expr decls l in
     let+ (decls, r) = trav_expr decls r in
     decls, BinOp (l, o, r)
  | UnOp (o, e) ->
     let+ (decls, e) = trav_expr decls e in
     decls, UnOp (o, e)
  | Index (a, b) ->
     let* (decls, a) = trav_expr decls a in
     let+ (decls, b) = trav_expr decls b in
     decls, Index (a, b)
  | StructMemberAccess (e, s) ->
     let+ (decls, e) = trav_expr decls e in
     decls, StructMemberAccess (e, s)
  | StructIndexAccess (e, i) ->
     let+ (decls, e) = trav_expr decls e in
     decls, StructIndexAccess (e, i)

  | TemplateInstance (name, types) ->
     let+ decls = create_decl name types decls in
     decls, Identifier (serialize_instance name types)

and trav_vd decls vd =
  let (name, expr) = match vd with
    | Val (name, _, expr) -> (name, expr)
    | ValI (name, expr) -> (name, expr)
    | Var (name, _, expr) -> (name, expr)
    | VarI (name, expr) -> (name, expr)
  in
  let+ (decls, expr) = trav_expr decls expr in
  decls,
  match vd with
  | Val (name, t, _) -> Val (name, t, expr)
  | ValI (name, _) -> ValI (name, expr)
  | Var (name, t, _) -> Var (name, t, expr)
  | VarI (name, _) -> VarI (name, expr)

and trav_stmt decls s = match s with
  | Empty | Continue | Break | Return None -> Ok (decls, s)
  | Decl vd ->
     let+ (decls, vd) = trav_vd decls vd in
     decls, Decl vd
  | Expr e ->
     let+ (decls, e) = trav_expr decls e in
     decls, Expr e
  | Block ss ->
     let+ (decls, ss) = Util.flb (add_object trav_stmt) (decls, []) ss in
     decls, Block (List.rev ss)
  | IfElse (e, s1, s2) ->
     let* (decls, e) = trav_expr decls e in
     let* (decls, s1) = trav_stmt decls s1 in
     let+ (decls, s2) = trav_stmt decls s2 in
     decls, IfElse (e, s1, s2)
  | While (e, s) ->
     let* (decls, e) = trav_expr decls e in
     let+ (decls, s) = trav_stmt decls s in
     decls, While (e, s)
  | For (vd, e1, e2, s) ->
     let* (decls, vd) = trav_vd decls vd in
     let* (decls, e1) = trav_expr decls e1 in
     let* (decls, e2) = trav_expr decls e2 in
     let+ (decls, s) = trav_stmt decls s in
     decls, For (vd, e1, e2, s)
  | Return (Some e) ->
     let+ (decls, e) = trav_expr decls e in
     decls, Return (Some e)

and trav_top_decl decls decl = match decl with
  | TypeDef (name, t) ->
     let+ (decls, t) = trav_type decls t in
     (name, TypeDef (name, t)) :: decls
  | TypeFwdDef name -> Ok ((name, decl) :: decls)

  | ValDecl (pub, vd) ->
     let (name, expr) = match vd with
       | Val (name, _, expr) -> (name, expr)
       | ValI (name, expr) -> (name, expr)
       | Var (name, _, expr) -> (name, expr)
       | VarI (name, expr) -> (name, expr)
     in
     let+ (decls, vd) = trav_vd decls vd in
     (name, ValDecl (pub, vd)) :: decls

  | FuncDecl (pub, fd) ->
     let (name, args, rettype, body) = fd in
     let* (decls, rettype) = trav_type decls rettype in
     let add_arg (decls, args) arg =
       let (name, t) = arg in
       let+ (decls, t) = trav_type decls t in
       (decls, (name, t) :: args)
     in
     let* (decls, args) = Util.flb add_arg (decls, []) args in
     let args = List.rev args in
     let+ (decls, body) = trav_stmt decls body in
     (name, FuncDecl (pub, (name, args, rettype, body))) :: decls
  | FuncFwdDecl (name, args, rettype, extern) ->
     let* (decls, rettype) = trav_type decls rettype in
     let add_arg (decls, args) arg =
       let (name, t) = arg in
       let+ (decls, t) = trav_type decls t in
       (decls, (name, t) :: args)
     in
     let+ (decls, args) = Util.flb add_arg (decls, []) args in
     let args = List.rev args in
     (name, FuncFwdDecl (name, args, rettype, extern)) :: decls

  | TemplateTypeDef (_, (name, _)) -> Ok ((name, decl) :: decls)
  | TemplateTypeFwdDef (_, name) -> Ok ((name, decl) :: decls)

  | TemplateFuncDecl (_, (_, (name, _, _, _))) -> Ok ((name, decl) :: decls)
  | TemplateFuncFwdDecl (_, (name, _, _, _)) -> Ok ((name, decl) :: decls)

let process_file f =
  let+ decls = Util.flb trav_top_decl [] f in
  (fun (_, a) -> a) @@ List.split @@ List.rev decls
