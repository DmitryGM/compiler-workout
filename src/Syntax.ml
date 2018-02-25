(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT 
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)
    
    let eval_op op v1 v2 =
    let intToBool num = if 0 != num then true else false in
    let boolToInt bol = if bol then 1 else 0 in
    match op with
    | "+"  -> ((+)v1 v2)
    | "-"  -> v1 - v2
    | "*"  -> v1 * v2
    | "/"  -> v1 / v2
    | "%"  -> v1 mod v2
    | "<"  -> boolToInt (v1 < v2)
    | ">"  -> boolToInt  (v1 > v2)
    | "<=" -> boolToInt (v1 <= v2)
    | ">=" -> boolToInt (v1 >= v2)
    | "==" -> boolToInt (v1 = v2)
    | "!=" -> boolToInt (v1 <> v2)
    | "&&" -> boolToInt ((intToBool v1) && (intToBool v2))
    | "!!" -> boolToInt ((intToBool v1) || (intToBool v2))
    | _ -> failwith (Printf.sprintf "Undefined operator %s" op) ;;

let rec eval state expr =
    match expr with
    | Const c -> c
    | Var v -> state v
    | Binop (op, e1, e2) ->
        let v1 = eval state e1 in
        let v2 = eval state e2 in
        eval_op op v1 v2 ;;    

  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let eval _ = failwith "Not implemented yet"
                                                         
  end
