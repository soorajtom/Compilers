(* The abstract syntax tree for expression *)

structure Ast =
struct

datatype BinOp = Plus | Minus | Mul;

(* The abstract syntax for expressions *)
datatype Expr  = Const of int
	       | Op    of Expr * BinOp * Expr;


(* meaning of binary operators *)
fun binOpDenote Plus  x y = x + y
  | binOpDenote Minus x y = x - y
  | binOpDenote Mul   x y = x * y;

fun exprDenote (Const x)       = x
  | exprDenote (Op (x,oper,y)) = binOpDenote oper (exprDenote x) (exprDenote y);


(* Conversion to strings *)

fun binOpToString Plus  = "+"
  | binOpToString Minus = "-"
  | binOpToString Mul   = "*"

end
