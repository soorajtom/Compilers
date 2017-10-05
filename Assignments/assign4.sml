(*Author : Sooraj Tom, 111501036
*)

signature Graph = sig
   eqtype node
   type   graph
   
   val empty   : graph
   val newNode : node          -> graph -> graph
   val addEdge : (node * node) -> graph -> graph
   val nodes   : graph         -> node list
   val succ    : node          ->graph  -> node list
   val pred    : node          ->graph  -> node list
end;

signature GraphType = sig
	eqtype nType;
end;

functor dirgraph( gType:GraphType ):Graph = 
struct
    
    type node = gType.nType;
    fun equal a b = (a = b);
    type graph = (node * (node list)) list;
        
    val empty = [];
    
    fun newNode a g = (a, []) :: g;
    
    fun addEdge (a, b) ((x, y) :: xs) = if(x = a) then (x , b :: y) :: xs 
    									else (x, y) :: (addEdge (a,b) xs)
       |addEdge _ _ = [];
    
    fun nodes ((x, y) :: xs) = x :: nodes xs
       |nodes _ = [];
       
	fun succ a ((x, y) :: g) = if (x = a) then y else succ a g
	   |succ _ _ = []
	
	fun pred a ((x, y) :: g) = if (List.exists (equal a) y) then (x :: (pred a g)) else pred a g
	   |pred _ _ = []
	
end;

structure intGtype:GraphType =
struct
	type nType = int;
end;

structure intG = dirgraph(intGtype);

val sampleg : intG.graph = [(1,[2]),(2,[3]),(3,[4]),(4,[5, 6]),(5,[6]),(6,[])];
