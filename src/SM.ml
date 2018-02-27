open GT       
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
let eval _ = failwith "Not yet implemented"
 *)

let get_1 (a,_) = a ;;
let get_2 (_,a) = a ;;

let rec eval config prg =
    let stack = get_1 config in
    let stmt_conf = get_2 config in
    let state = Syntax.Stmt.get_1 stmt_conf in
    let input = Syntax.Stmt.get_2 stmt_conf in
    let output = Syntax.Stmt.get_3 stmt_conf in
    if List.length prg = 0 then
        config
    else
        let insn::p = prg in
        match insn with
        | BINOP op -> 
            let y::x::st = stack in
            eval ([(Syntax.Expr.eval_op op x y)]@st, stmt_conf) p
        
        | CONST i -> eval ([i]@stack, stmt_conf) p

        | READ    -> 
            let h::t = input in
            eval ([h]@stack, (state, t, output)) p

        | WRITE   ->
            let h::st = stack in
            eval (st, (state, input, output@[h])) p

        | LD x    -> eval ([(state x)]@stack, (state, input, output)) p

        | ST x    ->
            let h::st = stack in
            eval (st, ((Syntax.Expr.update x h state), input, output)) p

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine

let compile _ = failwith "Not yet implemented"
*)

let rec compile_expr state expr =
    match expr with
    | Syntax.Expr.Const c -> [CONST c]
    | Syntax.Expr.Var x   -> [LD x]
    | Syntax.Expr.Binop (op, v1, v2) -> (compile_expr state v1) @ (compile_expr state v2) @ [BINOP op]

let get_1 (a,_,_) = a ;;
let get_2 (_,a,_) = a ;;
let get_3 (_,_,a) = a ;; 

let rec compile prog =
    let config = (Syntax.Expr.empty,[],[]) in
    let state = get_1 config in
    let input = get_2 config in
    let output = get_3 config in
    match prog with
    | Syntax.Stmt.Assign (x, e) -> (compile_expr state e)@[ST x]
    | Syntax.Stmt.Read x     -> [READ; ST x]
    | Syntax.Stmt.Write e    -> (compile_expr state e)@[WRITE]
    | Syntax.Stmt.Seq (s1, s2)  -> (compile s1) @ (compile s2)
;;





