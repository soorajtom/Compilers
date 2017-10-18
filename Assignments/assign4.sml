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

val sampleg : intG.graph = [(1,[2]),(2,[3,4]),(3,[]),(4,[])];
val graph1 : intG.graph = [(1,[2]),(2,[3]),(3,[4]),(4,[5, 2]),(5,[6, 7]),(6,[8]),(7,[9]),(8,[]),(9,[10,11]),(10,[]),(11,[])];
val graph2 : intG.graph = [ (1, [2]), (2, [3,4]), (3,[5]), (4, [6]), (5, [1]), (6, []) ];

fun extractLeadsSucc ((x, y) :: xs) g = if (List.length y) > 1 then y @ extractLeadsSucc xs g
								else(extractLeadsSucc xs g)
   |extractLeadsSucc _ _= [];

fun searchNode n ((x, y) :: xs) = if x = n then 0 else 1 + searchNode n xs
   |searchNode _ _ = 999;
   (*
fun getBlock (a:int) (g:((int * (int list)) list)) = 
					if (List.length ((List.nth g (searchNode a g))#2)) > 1 then [] 					
					else ( ((List.nth g (searchNode a g))#1) :: (getBlock (List.nth ((List.nth g (searchNode a g))#2) 0) g) )
   |getBlock _ _ = [];
*)

fun isaleader [x] (l :: leads) = if( x = l) then true else isaleader [x] leads
   |isaleader _ _ = false;

fun getBlock [a] (g:((int * (int list)) list)) lNodes= 
					let
					val index = searchNode a g;
					val nextnode:(int list) = intG.succ a g;
					(*val fnode = List.nth nextnode 0;*)
					in
					if(((List.length nextnode ) > 1) orelse (isaleader nextnode lNodes)) then [a]
					else [a] @ getBlock nextnode g lNodes
					end
   |getBlock _ _ _ = [];

fun getBasicBlocks (x :: leads) lNodes graph = [getBlock [x] graph lNodes] @ getBasicBlocks leads lNodes graph
   |getBasicBlocks _ _ _ = [];
   
fun main start graph = let
					val leaderNodes = (start :: (extractLeadsSucc graph graph));
					in
					getBasicBlocks leaderNodes leaderNodes graph
					end;





