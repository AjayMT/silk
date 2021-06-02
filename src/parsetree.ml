
(**
 * Parse tree definitions.
 *)

type literal = LI8 of int | LI16 of int | LI32 of int | LI64 of int
               | LU8 of int | LU16 of int | LU32 of int | LU64 of int
               | LF32 of float | LF64 of float
               | LBool of bool
               | LString of string
               | LNil

type bin_op = Plus | Minus | Times | Divide | Modulus
              | Equal | LessThan | GreaterThan
              | And | Or
              | RShift | LShift | BitAnd | BitOr | BitXor
type un_op = UMinus | Not | BitNot | AddressOf | Deref

type type_ = I8 | I16 | I32 | I64
             | U8 | U16 | U32 | U64
             | F32 | F64
             | Function of type_ list * type_
             | Pointer of type_ | MutPointer of type_
             | Array of int * type_
             | StructLabeled of bool * (string * type_) list
             | Struct of bool * type_ list
             | Void | Bool | TypeAlias of string
             | AliasTemplateInstance of string * type_ list
             | Template of string

type expr = Identifier of string | Literal of literal | Assignment of expr * expr
            | TemplateInstance of string * type_ list
            | StructLiteral of bool * expr list
            | StructInit of type_ * expr list
            | ArrayElems of expr list
            | ArrayInit of type_ * int
            | FunctionCall of expr * expr list
            | TypeCast of type_ * expr
            | BinOp of expr * bin_op * expr
            | UnOp of un_op * expr
            | Index of expr * expr
            | StructMemberAccess of expr * string
            | StructIndexAccess of expr * int

type val_decl = ValI of string * expr | Val of string * type_ * expr
                | VarI of string * expr | Var of string * type_ * expr

type type_def = string * type_
type template_type_def = string * string list * type_

type statement = Empty | Decl of val_decl | Expr of expr | Block of statement list
                 | IfElse of expr * statement * statement
                 | While of expr * statement
                 | For of val_decl * expr * expr * statement
                 | Continue | Break | Return of expr option

type func_fwd_decl = string * (string * type_) list * type_ * bool

type func_decl = string * (string * type_) list * type_ * statement

type top_decl = TypeDef of type_def
              | TypeFwdDef of string

              | ValDecl of bool * val_decl
              | FuncDecl of bool * func_decl
              | FuncFwdDecl of func_fwd_decl

              | TemplateTypeDef of (string list * type_def)
              | TemplateTypeFwdDef of (string list * string)
              | TemplateFuncDecl of bool * (string list * func_decl)
              | TemplateFuncFwdDecl of (string list * func_fwd_decl)


(**
 * show_x: Format x as string for template instantiation and error messages.
 *)

let rec show_type t = match t with
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
       (String.concat "," @@ List.map show_type ats)
       ^ ")" ^ (show_type rt)
  | Pointer t -> "*" ^ (show_type t)
  | MutPointer t -> "mut*" ^ (show_type t)
  | Array (i, t) -> "[" ^ (string_of_int i) ^ "]" ^ (show_type t)
  | Struct (packed, ts) ->
     let prefix = if packed then "(:" else "(" in
     let suffix = if packed then ":)" else ")" in
     prefix ^ (String.concat "," @@ List.map show_type ts) ^ suffix
  | StructLabeled (packed, pairs) ->
     let prefix = if packed then "(:" else "(" in
     let suffix = if packed then ":)" else ")" in
     let serialize_pair (n, t) = n ^ " " ^ (show_type t) in
     prefix ^ (String.concat "," @@ List.map serialize_pair pairs) ^ suffix
  | AliasTemplateInstance (name, ts) ->
     name ^ ":[" ^ (String.concat "," @@ List.map show_type ts) ^ "]"


let rec show_expr e = match e with
  | Identifier s -> s
  | Literal l ->
     begin match l with
     | LI8 i | LI16 i | LI32 i | LI64 i
       | LU8 i | LU16 i | LU32 i | LU64 i -> string_of_int i
     | LF32 f | LF64 f -> string_of_float f
     | LBool b -> string_of_bool b
     | LString s -> "\"" ^ s ^ "\""
     | LNil -> "nil"
     end
  | Assignment (e1, e2) -> (show_expr e1) ^ " = " ^ (show_expr e2)
  | TemplateInstance (name, ts) -> show_type @@ AliasTemplateInstance (name, ts)
  | StructLiteral (packed, es) ->
     let (pre, suf) = if packed then ("(:", ":)") else ("(", ")") in
     pre ^ (String.concat ", " @@ List.map show_expr es) ^ suf
  | StructInit (t, es) ->
     (show_type t) ^ "(" ^ (String.concat ", " @@ List.map show_expr es) ^ ")"
  | ArrayElems es -> "{" ^ (String.concat ", " @@ List.map show_expr es) ^ "}"
  | ArrayInit (t, i) -> "[" ^ (show_type t) ^ "; " ^ (string_of_int i) ^ "]"
  | FunctionCall (e, es) ->
     (show_expr e) ^ "(" ^ (String.concat ", " @@ List.map show_expr es) ^ ")"
  | TypeCast (t, e) -> (show_type t) ^ "(" ^ (show_expr e) ^ ")"
  | BinOp (e1, op, e2) ->
     let op_str = match op with
       | Plus -> " + " | Minus -> " - " | Times -> " * " | Divide -> " / "
       | Modulus -> " % "
       | Equal -> " == " | LessThan -> " < " | GreaterThan -> " > "
       | And -> " && " | Or -> " || "
       | RShift -> " >> " | LShift -> " << " | BitAnd -> " & " | BitOr -> " | "
       | BitXor -> " ^ "
     in
     "(" ^ (show_expr e1) ^ op_str ^ (show_expr e2) ^ ")"
  | UnOp (op, e) ->
     let op_str = match op with
       | UMinus -> "-" | Not -> "!" | BitNot -> "~"
       | AddressOf -> "&" | Deref -> "@"
     in
     op_str ^ (show_expr e)
  | Index (e1, e2) -> (show_expr e1) ^ "[" ^ (show_expr e2) ^ "]"
  | StructMemberAccess (e, s) -> (show_expr e) ^ "." ^ s
  | StructIndexAccess (e, i) -> (show_expr e) ^ "." ^ (string_of_int i)


let show_decl vd = match vd with
  | ValI (s, e) -> String.concat " " ["val"; s; "="; show_expr e]
  | Val (s, t, e) -> String.concat " " ["val"; s; show_type t; "="; show_expr e]
  | VarI (s, e) -> String.concat " " ["var"; s; "="; show_expr e]
  | Var (s, t, e) -> String.concat " " ["var"; s; show_type t; "="; show_expr e]


let rec show_stmt s =
  match s with
  | Empty -> ";"
  | Decl vd -> (show_decl vd) ^ ";"
  | Expr e -> (show_expr e) ^ ";"
  | Block stmts ->
     "{\n"
     ^ (String.concat "\n" @@ List.map (fun s -> "  " ^ show_stmt s) stmts)
     ^ "\n}"
  | IfElse (e, ifs, elses) ->
     "if " ^ (show_expr e) ^ " " ^ (show_stmt ifs) ^ " else " ^ (show_stmt elses)
  | While (e, stmts) -> "while " ^ (show_expr e) ^ " " ^ (show_stmt stmts)
  | For (vd, e1, e2, s) ->
     "for " ^ (show_decl vd) ^ "; " ^ (show_expr e1) ^ "; " ^ (show_expr e2)
     ^ " " ^ (show_stmt s)
  | Continue -> "continue;" | Break -> "break;"
  | Return e ->
     let e_str = match e with
       | Some e -> " " ^ (show_expr e) ^ ";"
       | None -> ";"
     in
     "return" ^ e_str


let rec show_top_decl td =
  let templatize_name n ts = n ^ ":[" ^ (String.concat ", " ts) ^ "]" in
  let show_args args =
    let show_arg (name, t) = name ^ " " ^ (show_type t) in
    String.concat ", " @@ List.map show_arg args
  in

  match td with
  | TypeDef (n, t) -> (String.concat " " ["type"; n; "="; show_type t]) ^ ";"
  | TypeFwdDef n -> "type " ^ n ^ ";"
  | ValDecl (p, vd) ->
     let p_str = if not p then "private " else "" in
     p_str ^ (show_decl vd) ^ ";"
  | FuncDecl (p, (name, args, ret, stmt)) ->
     let p_str = if not p then "private " else "" in
     p_str ^ name ^ "(" ^ (show_args args) ^ ") "
     ^ (show_type ret) ^ " " ^ (show_stmt stmt)
  | FuncFwdDecl (name, args, ret, ext) ->
     let ext_str = if ext then "extern " else "" in
     ext_str ^ name ^ "(" ^ (show_args args) ^ ") " ^ (show_type ret) ^ ";"
  | TemplateTypeDef (ts, (n, t)) -> show_top_decl @@ TypeDef (templatize_name n ts, t)
  | TemplateTypeFwdDef (ts, n) -> show_top_decl @@ TypeFwdDef (templatize_name n ts)
  | TemplateFuncDecl (p, (ts, (n, args, ret, stmt))) ->
     show_top_decl @@ FuncDecl (p, (templatize_name n ts, args, ret, stmt))
  | TemplateFuncFwdDecl (ts, (n, args, ret, ext)) ->
     show_top_decl @@ FuncFwdDecl (templatize_name n ts, args, ret, ext)
