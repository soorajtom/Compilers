(* The reverse polish machine

The execution of the machine is simple. It has a stack and as long as
it sees values, it pushes them. Once it sees an operator it pops the
appropriate number of elements from the stack, applies the operator,
and pushes the result.



*)
structure Machine =
struct

(* The instructions of the machine *)

datatype Inst    = Exec of Ast.BinOp | Push of int | Print;
type     Program = Inst list;


(*

Printing the program for use with another reverse polish calculator
like dc.

*)

fun opToString Ast.Plus  = "+"
  | opToString Ast.Minus = "-"
  | opToString Ast.Mul   = "*";

fun instToString (Exec oper) = opToString oper
  | instToString (Push x   ) = Int.toString x
  | instToString Print       = "p"

val toString = String.concatWith " " o List.map instToString;

(* Run the stack machine *)
type     Stack   = int list;

fun stackUnderflow stack = (print "error: stack underflow" ; OS.Process.exit (OS.Process.failure); stack)

fun printtop (top::rest) = (print (Int.toString top); top::rest)
  | printtop stack       = stackUnderflow stack


fun step (Push x)    stack            = x :: stack
  | step Print       stack            = printtop stack
  | step (Exec oper) (a :: b :: rest) = Ast.binOpDenote oper a b :: rest
  | step _           stack            = stackUnderflow stack

val run = List.foldl (fn (inst,stack) => step inst stack) []

end
