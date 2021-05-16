(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expression Syntaxe ML                                              == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type bop = Add | Mul | Sub | Div | Eq | Lt | And | Or 
and uop = Not

(* SType: Int | Bool en APS2 *)
and tprim = Int | Bool

and typ = Type of tprim | TypeFunc of typs * typ (*ARROW*) | VOID | TypeVec of typ
and typs = ASTType of typ | ASTTypes of typ * typs

and arg = Argu of string * typ
and args = ASTArg of arg | ASTArgs of arg * args

(* aps1a *)
(* string * string * typ: var ident type *)
and argp = ArguP of string * typ | ArguPA of string * typ
and argsp = ASTArgP of argp | ASTArgsP of argp * argsp

(* APS2 *)
and lval = ASTLvId of string | ASTLval of lval * expr

and expr =
    ASTNum of int
  | ASTId of string
  | ASTApp of expr * exprs
  | ASTBool of bool
  | ASTIf of expr * expr * expr
  | ASTBinary of bop * expr * expr
  | ASTUnary of uop * expr
  | ASTFunc of args * expr
  (* APS2 *)
	|ASTNth of expr * expr
	|ASTLen of expr
	|ASTAlloc of expr 
and exprs =
    ASTExpr of expr 
  | ASTExprs of expr * exprs
and exprp = 
    ASTPr of expr
  | ASTPrCall of expr
and exprsp =
    ASTExprp of exprp
  | ASTExprsp of exprp * exprsp

and stat = 
    ASTEcho of expr
  (* APS1 *)
  (* SET de APS2 *)
  | ASTSet of lval * expr
  | ASTIfb of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of expr * exprsp

and dec = 
    ASTConst of string * typ * expr
  | ASTFun of string * typ * args * expr
  | ASTFunRec of string * typ * args * expr
  (* APS1 *)
  | ASTVar of string * typ
  | ASTProc of string * argsp * block
  | ASTProcRec of string * argsp * block

(* APS1 *)
and block =
  ASTBlock of cmds

and cmds = 
    ASTStat of stat
  | ASTDec of dec * cmds
  | ASTStatCmd of stat * cmds

type prog = 
  ASTProg of cmds

let bop_of_string bop =
  match bop with
    "add" -> Add
  | "mul" -> Mul
  | "sub" -> Sub
  | "div" -> Div
  | "and" -> And
  | "or" -> Or
  | "eq" -> Eq
  | "lt" -> Lt
  | _ -> failwith "not an operator"

let string_of_bop bop = 
  match bop with
    Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | Div -> "div"
  | And -> "and"
  | Or -> "or"
  | Eq -> "eq"
  | Lt -> "lt"

let uop_of_string uop =
  match uop with
    "not" -> Not
  | _ -> failwith "not an operator"

let string_of_uop uop =
  match uop with
    Not -> "not"
    
let string_of_tprim tprim =
(*print a trim in prolog*)
  match tprim with
    Int -> "int"
  | Bool -> "bool"
