
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
                 (* (nonTer("A"),[nonTerm("B"), term("+")]),*)
                  (nonTer("B"),[term("+"), nonTerm("B")]),
                  (nonTer("B"),[term("-"), term("b")]) ] , "A" );

fun equal x y = if x = y then true else false;
fun notequal x y = if x <> y then true else false;

fun getLhs(x : lhs * rhs):lhs = #1x;   (*Extracting lhs of production*)
fun getRhs(x : lhs * rhs):rhs = #2x;   (*Extracting rhs of production*)

fun extractSymbolRhs(term(x)) = x
   |extractSymbolRhs(nonTerm(x)) = x
   |extractSymbolRhs(epsilon) = "";

fun extractSymbolLhs(nonTer(x)) = x;

fun extractLhs(prod) = List.map getLhs prod;  (*extracting all lhs out of production rules*)

(*returns a list with duplicate elements removed*)
fun copylist(x :: a, b) = if List.exists (equal x) b then copylist(a,b) else [x] @ copylist(a,b @ [x])
   |copylist(x, b)  = x;

fun getNonTerms(gram:Gram) = List.map extractSymbolLhs (copylist( extractLhs(#1gram), [] ));

val NonTerms = getNonTerms(gra);

fun extractRhs(prod) = List.map getRhs prod;

fun extractRhsList(x :: y)= x @ extractRhsList(y)
   |extractRhsList([]) = [];

fun extractTerm(term(x) :: y) = [term(x)] @ extractTerm(y)
   |extractTerm (x :: y) = [] @ extractTerm(y)
   |extractTerm (x) = [];

val Terms = List.map extractSymbolRhs (copylist(extractTerm(extractRhsList(extractRhs(#1gra))), [] ));

structure strKey:ORD_KEY =
	struct
		type ord_key = string;
		val compare = String.compare;
	end;

structure strmap = ListMapFn(strKey);

type maptype = string list list strmap.map;
(*fun createmap(): rhs list strmap.map = strmap.empty;*)

fun getRhsWithoutEps(prod) = if ((List.map extractSymbolRhs prod) = [""]) then ([[]]) else ([List.map extractSymbolRhs prod]);

fun addToMap( m: maptype, NonT:string, prod:rhs):maptype = if strmap.inDomain(m, NonT)
			then strmap.insert(m, NonT, strmap.lookup(m, NonT) @ getRhsWithoutEps(prod))
			else strmap.insert(m, NonT, getRhsWithoutEps(prod));

val s: maptype = strmap.empty;

fun addProdToMap(m: maptype, Prod: production):maptype =
	addToMap(m, extractSymbolLhs(#1Prod), #2Prod);

fun makeMapofProdrules(m: maptype,x :: Prods: production list):maptype = makeMapofProdrules(addProdToMap(m, x), Prods)
   |makeMapofProdrules(m: maptype, []:production list):maptype = m;

val s = makeMapofProdrules(s, #1gra);

fun expandNT(m:maptype, x:string) = strmap.lookup(m, x);

(*initialising nullable, first and follow maps*)
fun initmaps _ = 
let
	val ncount = ref (List.length NonTerms);
	val tcount = ref (List.length Terms);
	val nullable = ref (strmap.empty:bool strmap.map);
	val FIRST = ref (strmap.empty:string list strmap.map);
	val FOLLOW = ref (strmap.empty:string list strmap.map);
in
	while (!ncount > 0) do(
		nullable := strmap.insert((!nullable), List.nth (NonTerms, (!ncount - 1)), false);
		FIRST := strmap.insert((!FIRST), List.nth (NonTerms, (!ncount - 1)), []);
		FOLLOW := strmap.insert((!FOLLOW), List.nth (NonTerms, (!ncount - 1)), []);
		ncount := !ncount - 1
	);
	while (!tcount > 0) do(
		nullable := strmap.insert((!nullable), List.nth (Terms, (!tcount - 1)), false);
		FIRST := strmap.insert((!FIRST), List.nth (Terms, (!tcount - 1)), [List.nth (Terms, (!tcount - 1))]);
		tcount := !tcount - 1
	);
	(!nullable, !FIRST, !FOLLOW)
end;

val (nullable, FIRST, FOLLOW) = initmaps 0;

fun isNull( x :: y) = strmap.lookup(nullable, x) andalso isNull(y)
   |isNull( [] ) = true;
(*fun isNullable( x :: y) = isNull(x) andalso isNullable(y)
   |isNullable( [] ) = true;*)
			   
fun maketables m =
let
	val ncount = ref (List.length NonTerms);
	val pi = ref 0;
	val k = ref 0;
	val i = ref 0;
	val nu = ref (strmap.empty:bool strmap.map);
	val FI = ref (strmap.empty:string list strmap.map);
	val FO = ref (strmap.empty:string list strmap.map);
	val nullable = ref nullable;
	val FIRST = ref FIRST;
	val FOLLOW = ref FOLLOW;
	val prodlist = ref [[""]];
	val prod = ref [""];
	
in
	while((strmap.listItemsi(!nu) <> (strmap.listItemsi(!nullable)))andalso((strmap.listItemsi(!FI)) <> (strmap.listItemsi(!FIRST)))andalso((strmap.listItemsi(!FO)) <> (strmap.listItemsi(!FOLLOW))))do
	(
		nu := !nullable;
		FI := !FIRST;
		FO := !FOLLOW;
		ncount := (List.length NonTerms);
		while (!ncount > 0) do                            (*for each nonTerm X*)
		(
			prodlist := expandNT(m, List.nth (NonTerms, (!ncount - 1)));
			pi := 0;
			while (!pi < (List.length (!prodlist))) do    (* for each production X -> Y1....Yk*)
			(
				prod := List.nth (!prodlist, !pi);
				nullable := strmap.insert(!nullable, (List.nth (NonTerms, (!ncount - 1))), strmap.lookup(!nullable, (List.nth (NonTerms, (!ncount - 1)))) orelse isNull(!prod));
				
				k := List.length (!prod);
				i := 1;
				while(!i <= !k)do
				(
					
					FIRST := strmap.insert(!FIRST, (List.nth (NonTerms, (!ncount - 1))) ,
					if(isNull(List.take(!prod, !i - 1)))
					then(strmap.lookup(!FIRST, (List.nth (NonTerms, (!ncount - 1)))) @ strmap.lookup(!FIRST, (List.nth (!prod, (!i - 1)))))
					else(strmap.lookup(!FIRST, (List.nth (NonTerms, (!ncount - 1))))));
					
					FOLLOW := strmap.insert(!FOLLOW, (List.nth (NonTerms, (!ncount - 1))) ,
					if(isNull(List.take(!prod, !i - 1)))
					then(strmap.lookup(!FOLLOW, (List.nth (NonTerms, (!ncount - 1)))) @ strmap.lookup(!FOLLOW, (List.nth (!prod, (!i - 1)))))
					else(strmap.lookup(!FOLLOW, (List.nth (NonTerms, (!ncount - 1))))));
					i := !i + 1
				);
				
				pi := !pi + 1	
			);
			ncount:= !ncount - 1
		)
	);
	(!nullable, !FIRST, !FOLLOW)
end;

val (nullable, FIRST, FOLLOW) = maketables s;


(*fun getheads(m: maptype, x :: r :string list list) =
                           (if (List.exists (equal (hd x)) NonTerms)
                           then (getheads(m, expandNT(m, (hd x)))) else [hd x]) @ getheads(m,r)
   |getheads(m: maptype, []: string list list) = [];

fun getheads(m: maptype, x :: r :string list list) = [hd x] @ getheads(m,r)
   |getheads(m: maptype, []: string list list) = [];



fun isNullable(m:maptype, a:string):bool = case List.find (equal "") (getheads(m, expandNT(m, a))) of SOME x => true | NONE => false;

fun first(m:maptype, a:string):string list = List.filter (notequal "") (getheads(m, expandNT(m, a)));
*)
