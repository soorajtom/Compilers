
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
val gra:Gram = ( [(nonTer("A"),[term("a"), nonTerm("B")]), 
				(nonTer("A"),[epsilon]), 
				(nonTer("B"),[term("+"), nonTerm("B")]) ] , "A" );

fun equal x y = if x = y then true else false;

fun getLhs(x : lhs * rhs):lhs = #1x;   (*Extracting lhs of production*)
fun getRhs(x : lhs * rhs):rhs = #2x;   (*Extracting rhs of production*)

(*fun extractSymbolRhs(term(x)) = x
   |extractSymbolRhs(nonTerm(x)) = x
   |extractSymbolRhs(epsilon) = "";*)

fun extractSymbolLhs(nonTer(x)) = x;

fun extractLhs(prod) = List.map getLhs prod;  (*extracting all lhs out of production rules*)

(*returns a list with duplicate elements removed*)
fun copylist(x :: a, b) = if List.exists (equal x) b then copylist(a,b) else [x] @ copylist(a,b @ [x])
   |copylist(x, b)  = x;

fun getNonTerms(gram:Gram) = List.map extractSymbolLhs (copylist( extractLhs(#1gram), [] ));

val NonTerms = getNonTerms(a);

fun extractRhs(prod) = List.map getRhs prod;

fun extractRhsList(x :: y)= x @ extractRhsList(y)
   |extractRhsList([]) = [];
   
fun extractTerm(term(x) :: y) = [term(x)] @ extractTerm(y)
   |extractTerm (x :: y) = [] @ extractTerm(y)
   |extractTerm (x) = [];

val Terms = copylist(extractTerm(extractRhsList(extractRhs(#1a))), [] );

structure strKey:ORD_KEY = 
	struct
		type ord_key = string;
		val compare = String.compare;
	end;

structure strmap = ListMapFn(strKey);

type maptype = rhs list strmap.map;
(*fun createmap(): rhs list strmap.map = strmap.empty;*)

fun addToMap( m: maptype, NonT:string, prod:rhs):maptype = if strmap.inDomain(m, NonT) 
			then strmap.insert(m, NonT, strmap.lookup(m, NonT) @ [prod])
			else strmap.insert(m, NonT, [prod]);

val s: maptype = strmap.empty;

fun addProdToMap(m: maptype, Prod: production):maptype =
	addToMap(m, extractSymbolLhs(#1Prod), #2Prod);

fun makeMapofProdrules(m: maptype,x :: Prods: production list):maptype = makeMapofProdrules(addProdToMap(m, x), Prods)
   |makeMapofProdrules(m: maptype, []:production list):maptype = m;
	
fun isNullable(x:string) = 

(*
( [(nonTer("A"),[term("a"), nonTerm("b")]), (nonTer("A"),[term("a"), nonTerm("*")]), (nonTer("B"),[term("+"), nonTerm("b")]) ] , "A" )
( [("A",["a"]) ] , "A" )
( [(A,[a, b]) ] , A )

*)
