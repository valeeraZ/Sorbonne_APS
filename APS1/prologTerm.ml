(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expressions Syntaxe ML                                             == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast

let rec print_type t = 
  match t with
    Type(tprim) -> Printf.printf "%s" (string_of_tprim tprim)
  | TypeFunc(typs, typ) -> (
      Printf.printf "typeFunc";
      Printf.printf "(";
      Printf.printf "[";
      print_types typs;
      Printf.printf "]";
      Printf.printf ",";
      print_type typ;
      Printf.printf ")"
    )
and print_types t = 
  match t with
    ASTType(typ) -> print_type typ
  | ASTTypes(typ, typs) -> (
      print_type typ;
      Printf.printf ",";
      print_types typs;
    )

let print_arg a = 
  match a with
    Argu(label, typ) -> (
      Printf.printf "(";
      Printf.printf "%s" label;
      Printf.printf ",";
      print_type typ;
      Printf.printf ")"
    )
let rec print_args a = 
  match a with
    ASTArg(argu) -> print_arg argu
  | ASTArgs(argu, argus) -> (
      print_arg argu;
      Printf.printf ",";
      print_args argus;
    )

let rec print_expr e =
  match e with
    ASTNum n -> Printf.printf"num(%d)" n
  | ASTId x -> Printf.printf"id(%s)" x
  | ASTApp(e, es) -> (
      Printf.printf "app(";
      print_expr e;
      Printf.printf ",[";
      print_exprs es;
      Printf.printf "])"
    )
  | ASTBool(b) -> Printf.printf "%b" b
  | ASTIf(cond, cons, alt) -> (
      Printf.printf "if";
      Printf.printf "(";
      print_expr cond;
      Printf.printf ",";
      print_expr cons;
      Printf.printf ",";
      print_expr alt;
      Printf.printf ")";
    )
  | ASTBinary(op, e1, e2) ->(
      Printf.printf "%s" (string_of_bop op);
      Printf.printf "(";
      print_expr e1;
      Printf.printf ",";
      print_expr e2;
      Printf.printf ")";
    )
  | ASTUnary(op, e) -> (
      Printf.printf "%s" (string_of_uop op);
      Printf.printf "(";
      print_expr e;
      Printf.printf ")";
    )
  | ASTFunc(args, e) -> (
      Printf.printf "func";
      Printf.printf "(";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
and print_exprs es =
  match es with
    ASTExpr(e) -> print_expr e
  | ASTExprs(e, es) -> (
      print_expr e;
      Printf.printf ",";
      print_exprs es;
    )

let rec print_stat s = 
  match s with
    ASTEcho(e) -> (
      Printf.printf "echo";
      Printf.printf "(";
      print_expr e;
      Printf.printf ")"
    )
  (*APS1*)
  | ASTSet(label, e) -> (
      Printf.printf "set";
      Printf.printf "(";
      Printf.printf "%s" label;
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
  | ASTIfb(cond, b1, b2) -> (
      Printf.printf "ifb";
      Printf.printf "(";
      print_expr cond;
      Printf.printf ",";
      print_block b1;
      Printf.printf ",";
      print_block b2;
      Printf.printf ")";
    )
  | ASTWhile(cond, b) -> (
      Printf.printf "while";
      Printf.printf "(";
      print_expr cond;
      Printf.printf ",";
      print_block b;
      Printf.printf ")";
    )
  | ASTCall(label, exprs) -> (
      Printf.printf "call";
      Printf.printf "(";
      print_expr label;
      Printf.printf ",";
      Printf.printf "[";
      print_exprs exprs;
      Printf.printf "]";
      Printf.printf ")";
    )

and print_dec d = 
  match d with
    ASTConst(label, typ, e) -> (
      Printf.printf "const";
      Printf.printf "(";
      Printf.printf "%s" label;
      Printf.printf ",";
      print_type typ;
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
  | ASTFun(label, typ, args, e) -> (
      Printf.printf "fun";
      Printf.printf "(";
      Printf.printf "%s" label;
      Printf.printf ",";
      print_type typ;
      Printf.printf ",";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
  | ASTFunRec(label, typ, args, e) -> (
      Printf.printf "funRec";
      Printf.printf "(";
      Printf.printf "%s" label;
      Printf.printf ",";
      print_type typ;
      Printf.printf ",";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_expr e;
      Printf.printf ")";
    )
  (*APS1*)
  | ASTVar(label, typ) -> (
      Printf.printf "var";
      Printf.printf "(";
      Printf.printf "%s" label;
      Printf.printf ",";
      print_type typ;
      Printf.printf ")";
    )
  | ASTProc(label, args, block) -> (
      Printf.printf "proc";
      Printf.printf "(";
      Printf.printf "%s" label;
      Printf.printf ",";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_block block;
      Printf.printf ")";
    )
  | ASTProcRec(label, args, block) -> (
      Printf.printf "procRec";
      Printf.printf "(";
      Printf.printf "%s" label;
      Printf.printf ",";
      Printf.printf "[";
      print_args args;
      Printf.printf "]";
      Printf.printf ",";
      print_block block;
      Printf.printf ")";
    )

and print_cmds c = 
  match c with
    ASTStat(s) -> (
      Printf.printf "stat";
      Printf.printf "(";
      print_stat s;
      Printf.printf ")";
      )
  | ASTDec(d, c) -> (
      Printf.printf "dec";
      Printf.printf "(";
      print_dec d;
      Printf.printf ")";
      Printf.printf ",";
      print_cmds c;
    )
  | ASTStatCmd(s, c) -> (
    Printf.printf "stat";
    Printf.printf "(";
    print_stat s;
    Printf.printf ")";
    Printf.printf ",";
    print_cmds c;
  )

(*APS1*)
and print_block block =
  match block with
    ASTBlock(cmds) -> (
      Printf.printf "block";
      Printf.printf "(";
      Printf.printf "[";
      print_cmds cmds;
      Printf.printf "]";
      Printf.printf ")";
    )

let print_prog p = 
  match p with
    ASTProg(c) -> (
      Printf.printf "prog";
      Printf.printf "(";
      Printf.printf "[";
      print_cmds c;
      Printf.printf "]";
      Printf.printf ")";
      Printf.printf ".";
    )
;;

let file = open_in Sys.argv.(1)
let _ =
 try
   let lexbuf = Lexing.from_channel file in
   let e = Parser.prog Lexer.token lexbuf in
	    print_prog e;
	    print_char '\n'
 with Lexer.Eof -> exit 0

