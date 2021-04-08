open Ast;;

(*N, F, FR*)
(*E = ident -> V*)
type value = InN of int | InF of expr * string list * ident list | InFR of value | None
(*association id-value*)
and ident = Pair of string * value

let rec mem_env id liste =
  match liste with
    Pair(i,v)::tl -> if (String.equal id i) then true else mem_env id tl
  | [] -> false
;;

let rec extract_from_env id liste =
  match liste with
    Pair(i,v)::tl -> if (String.equal id i) then v else extract_from_env id tl
  | _ -> failwith "error extraction variable"
;;

let is_bop op = 
  match op with
    Add -> true
  | Mul -> true
  | Sub -> true
  | Div -> true
  | And -> true
  | Or -> true
  | Eq -> true
  | Lt -> true

let is_uop op =
  match op with
    Not -> true

let get_int value = 
  match value with
    InN(n) -> n
  | _ -> failwith "not N"

let get_ident arg = 
  match arg with
    Argu(id, typ) -> id

let rec make_closure args env = 
  match args with
    ASTArgs(arg, args) -> make_closure args [(get_ident arg)]@env
  | ASTArg(arg) -> [(get_ident arg)]@env

let eval_bop bop e1 e2 =
  match bop with
    Add -> InN((get_int e1) + (get_int e2))
  | Mul -> InN((get_int e1) * (get_int e2))
  | Sub -> InN((get_int e1) - (get_int e2))
  | Div -> InN((get_int e1) / (get_int e2))
  | And -> if (((get_int e1) + (get_int e2)) > 2 || (abs ( (get_int e1) - (get_int e2) )) < 0) then failwith "not a logical argument"
    else if (get_int e1) = 0 then InN(0) else InN((get_int e2))
  | Or -> if (((get_int e1) + (get_int e2)) > 2 || (abs ( (get_int e1) - (get_int e2) )) < 0) then failwith "not a logical argument"
    else if (get_int e1) = 1 then InN(1) else InN((get_int e2))
  | Eq -> if (get_int e1) = (get_int e2) then InN(1) else InN(0)
  | Lt -> if (get_int e1) < (get_int e2) then InN(1) else InN(0)

let eval_uop op e =
  match op with
    Not -> if (get_int e) = 0 then InN(1) 
    else if (get_int e) = 1 then InN(0) 
    else failwith "not valid argument"

let assoc_val closure env =
  List.map2 (function a -> function e -> Pair(a,e) ) closure env

let rec eval_expr expr env = 
  match expr with
  | ASTNum(n) -> InN(n)
  | ASTId(id) -> if mem_env id env then extract_from_env id env else failwith (id^" not a variable in environment")
  | ASTBinary(op,e1,e2) -> if is_bop op then eval_bop op (eval_expr e1 env) (eval_expr e2 env) 
    else failwith "not a binary operator"
  | ASTUnary(op,e) -> if is_uop op then eval_uop op (eval_expr e env) 
    else failwith "not an unary operator"
  | ASTIf(e1,e2,e3) -> if (eval_expr e1 env) = InN(1) then eval_expr e2 env 
    else if (eval_expr e1 env) = InN(0) then (eval_expr e3 env) 
    else failwith "not a boolean value"
  | ASTFunc(args,e_prim) -> InF(e_prim, (make_closure args []),env)
  | ASTBool(e) -> if e then InN(1) else InN(0)
  | ASTApp (expr,exprs) -> 
    match (eval_expr expr env) with
      InF(e_prim,closure,envi) -> let env_fun = (assoc_val closure (get_eval exprs env))@envi in
      eval_expr e_prim env_fun
    | InFR(closure) ->( match closure with
          InF(e_prim,f,envi) -> let env_fun = (assoc_val f (get_eval exprs env))@env in
          eval_expr (ASTApp(e_prim,exprs)) env_fun
        | _ -> failwith "not a recursive function")
    | InN(n) -> InN(n)
    | _ -> failwith "application result not applied yet"
and get_eval exprs env =
  match exprs with
    ASTExprs(e,es) -> (eval_expr e env)::(get_eval es env)
  | ASTExpr(e) -> (eval_expr e env)::[]

let eval_dec dec env=
  match dec with
  | ASTConst(id,t,expr) -> (Pair(id,(eval_expr expr env))::env)
  | ASTFun(id,t,args,expr) -> (Pair(id,InF(expr,make_closure args [],env))::env)
  | ASTFunRec(id,t,args,expr) -> (Pair(id, InFR(InF(expr,make_closure args [],env)))::env)

let eval_stat stat env output =
  match stat with
  | ASTEcho(n) -> (eval_expr n env)::output

let rec eval_cmds cmds env output =
  match cmds with
    ASTDec(dec,c) -> eval_cmds c (eval_dec dec env) output
  | ASTStatCmd(stat,c) -> eval_cmds c env (eval_stat stat env output)
  | ASTStat(stat) -> eval_stat stat env output

let print_val value =
  match value with
    InN(n) -> Printf.printf "%d\n" n
  | _ -> failwith "not a printable value"

let rec print_output output =
  List.iter (function x -> print_val x) (List.rev output) 

let eval_prog prog =
  match prog with
    ASTProg(cmds) -> print_output (eval_cmds cmds [] [])

let _ =
	try
		let fl = open_in Sys.argv.(1) in
		let lexbuf = Lexing.from_channel fl in
		let p = Parser.prog Lexer.token lexbuf in
			(eval_prog p)
	with Lexer.Eof -> exit 0