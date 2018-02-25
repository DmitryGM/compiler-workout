(* A deep embedding of simple expressions in OCaml. *)

(* Opening GT yet again. *)
open GT
       
(* Opening the substrate module for convenience. *)
open Syntax

(* Shortcuts for leaf constructors *)
let ( ! ) x = Expr.Var x
let ( !? ) n = Expr.Const n

(* Implementation of operators *)
let binop op x y = Expr.Binop (op, x, y)
                         
let ( +  ) = binop "+"
let ( -  ) = binop "-"
let ( *  ) = binop "*"
let ( /  ) = binop "/"
let ( %  ) = binop "%"
let ( <  ) = binop "<"
let ( <= ) = binop "<="
let ( >  ) = binop ">"
let ( >= ) = binop ">="
let ( == ) = binop "=="
let ( != ) = binop "!="
let ( && ) = binop "&&"
let ( || ) = binop "!!"

<<<<<<< HEAD
(*
=======
let ( =:= ) x e = Stmt.Assign (x, e)
let read  x = Stmt.Read x
let write e = Stmt.Write e
let (|>) x y = Stmt.Seq (x, y)
                             
>>>>>>> 3c0884cfa554ff5389a452d11065233a40863d0f
(* Some predefined names for variables *)
let x = !"x"
let y = !"y"
let z = !"z"
let t = !"t"

<<<<<<< HEAD
(* Voila; comment this out before submitting the solution *)
let _ =
  List.iter (fun e -> Printf.printf "eval s (%s) = %d\n" (show(expr) e) (eval s e)) [x+y*z- !?3; t-z+y && x]

*)
=======
                   
>>>>>>> 3c0884cfa554ff5389a452d11065233a40863d0f
