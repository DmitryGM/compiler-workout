open GT       
open Language
       
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
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
let eval _ = failwith "Not yet implemented"
 *)

let rec eval config prg =
    let (stack, stmt_conf) = config in
    let (state, input, output) = stmt_conf in
    if List.length prg = 0 then
        config
    else
        let insn::p = prg in
        match insn with
        | BINOP op -> 
            let y::x::st = stack in
            eval ([(Expr.eval_op op x y)]@st, stmt_conf) p
        
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
            eval (st, ((Expr.update x h state), input, output)) p

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Expr.empty, i, [])) p in o

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine

let compile _ = failwith "Not yet implemented"
*)

let rec compile_expr state expr =
    match expr with
    | Expr.Const c -> [CONST c]
    | Expr.Var x   -> [LD x]
    | Expr.Binop (op, v1, v2) -> (compile_expr state v1) @ (compile_expr state v2) @ [BINOP op]

let rec compile prog =
    let config = (Expr.empty,[],[]) in
    let (state, input, output) = config in
    match prog with
    | Stmt.Assign (x, e) -> (compile_expr state e)@[ST x]
    | Stmt.Read x     -> [READ; ST x]
    | Stmt.Write e    -> (compile_expr state e)@[WRITE]
    | Stmt.Seq (s1, s2)  -> (compile s1) @ (compile s2)
;;
