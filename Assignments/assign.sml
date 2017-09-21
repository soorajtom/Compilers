
datatype term = a of string | b of string;
datatype nonTerm = A of string;



datatype symbol = term | nonTerm ;

type lhs = nonTerm;

type rhs = (symbol) list;

type production = lhs * rhs;

type startSymbol = nonTerm;

type Grammar = (production) list * (startSymbol);


(*
( [(A,[a, b]) ] , A )
( [(A,[a, b]) ] , A )

*)
