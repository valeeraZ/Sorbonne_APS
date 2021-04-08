(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                     == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == S-expressions Syntaxe ML                                             == *)
(* == Fichier: lexer.mll                                                   == *)
(* ==  Lexique                                                             == *)
(* ========================================================================== *)

{
  open Parser
  exception Eof
}

rule token = parse
    [' ' '\t' '\n'] { token lexbuf } (* skip blanks *)
    | ('-'?)['0'-'9']+ as lxm { NUM(int_of_string lxm) }
    | "true" { TRUE }
    | "false" {FALSE}
    | "add" { PLUS }
    | "sub" { MINUS }
    | "mul" { TIMES }
    | "div" { DIV }
    | "and" { AND}
    | "or" { OR }
    | "eq" { EQ }
    | "lt" { LT }
    | "not" { NOT }
    | "int" { INT }
    | "bool" { BOOL }
    | "if" { IF }
    | "ECHO" { ECHO }
    | "CONST" { CONST }
    | "FUN" { FUN }
    | "REC" { REC }
    | '(' { LPAR }
    | ')' { RPAR }
    | '[' { LCRO }
    | ']' { RCRO }
    | ':' { COLON }
    | ';' { SEMICOLON }
    | ',' { COMA }
    | "->" { ARROW }
    | '*' {STAR}
    | eof { raise Eof }
    | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { IDENT(id)}