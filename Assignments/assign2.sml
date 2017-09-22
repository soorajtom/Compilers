
(*datatype term = a of string | b of string;
datatype nonTerm = A of string;*)

(*type term = string;
type nonTerm = string;*)

datatype symbol = term of string | nonTerm of string | epsilon;
type rhs = (symbol list);
datatype lhs = nonTer of string;
type production = lhs * rhs;
(*type startSymbol = nonTerm;*)
type Gram = (production list) * (string);
val a:Gram = ( [(nonTer("A"),[term("a"), nonTerm("b")]), 
				(nonTer("A"),[epsilon]), 
				(nonTer("B"),[term("+"), nonTerm("b")]) ] , "A" );

fun equal x y = if x = y then true else false;

fun getFirst(x : lhs * rhs):lhs = #1x;   (*Extracting lhs of production*)

fun extractNonterm(prod) = List.map getFirst prod;  (*extracting all lhs out of production rules*)

(*returns a list with duplicate elements removed*)
fun copylist(x :: a, b) = if List.exists (equal x) b then copylist(a,b) else [x] @ copylist(a,b @ [x])
   |copylist(x, b)  = x;


(*fun extractNonTerm(x :: prod:(production list)):list = [#1x] @ extractNonTerm(prod)
   |extractNonTerm k = k;*)
	 



(*
( [(nonTer("A"),[term("a"), nonTerm("b")]), (nonTer("A"),[term("a"), nonTerm("*")]), (nonTer("B"),[term("+"), nonTerm("b")]) ] , "A" )
( [("A",["a"]) ] , "A" )
( [(A,[a, b]) ] , A )

*)
