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
	
	type t = real
	val le: t -> t -> bool =fn a => fn b => (a <= b);

end


signature sort =
sig
	type a
	val qsort : a -> a
end

(* functor : structure * structure -> structure *)

functor Qsort (IntORD : ORD, 

struct
	type a = List;
	fun qsort (a
end



