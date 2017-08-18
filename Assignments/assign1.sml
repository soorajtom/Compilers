signature ORD =
sig 
	type t
	val le : t -> t -> bool
end

structure IntORD : ORD = 
struct

	type t = int;
	fun le a b = (a <= b);	

end

structure RealORD : ORD =
struct
	
	type t = real;
	val le: t -> t -> bool =fn a => fn b => (a <= b);

end


signature Sort =
sig
	type ltype
	val sort : ltype -> ltype
end

(* functor : structure * ... * structure -> structure *)

functor Qsort (z : ORD): Sort =

struct
	type ltype = z.t list;
	fun sort (x :: xs):ltype = let val (a, b) = 
			List.partition (z.le x) xs in sort b @ [x] @ sort a 
			end
	   |sort k:ltype = k;
	
end

structure intsort = Qsort(IntORD);











