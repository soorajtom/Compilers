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

fun equal a b = (a = b);

functor dirgraph( gType:GraphType ):Graph = 
struct
    
    type node = gType.nType;
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
val graph0 : intG.graph = [(1,[2]),(2,[3]),(3,[4]),(4,[5, 6]),(5,[7]),(6,[]),(7,[])];
val graph1 : intG.graph = [(3,[4]),(7,[9]),(9,[10,11]),(1,[2]),(2,[3]),(4,[5, 2]),(5,[6, 7]),(6,[8]),(8,[]),(10,[]),(11,[])];
val graph2 : intG.graph = [ (1, [2]), (2, [3,4]), (3,[5]), (4, [6]), (5, [1]), (6, []) ];
val graph3 : intG.graph = [ (1,[2]),(2,[3]),(3,[4,1]),(4,[5,1]),(5,[])];
val graph4 : intG.graph = [(1, [2,6]),(2, [3]),(3, [4]),(4, []), (5, [4]), (6, [5])];
val graph5 : intG.graph = [(1,[2]),(2,[3]),(3,[1]),(4,[3,1])];

fun refineList(x :: a) = if List.exists (equal x) a then refineList(a) else [x] @ refineList(a)
   |refineList(x)  = x;

fun extractLeadsSucc ((x, y) :: xs) = if (List.length y) > 1 then y @ extractLeadsSucc xs
								else(extractLeadsSucc xs)
   |extractLeadsSucc _ = [];
   
fun extractLeadsPred ((x,y) :: xs) g = if (List.length (intG.pred x g)) <> 1 then x :: (extractLeadsPred xs g)
								else(extractLeadsPred xs g)
   |extractLeadsPred _ _= [];

fun searchNode n ((x, y) :: xs) = if x = n then 0 else 1 + searchNode n xs
   |searchNode _ _ = ~999;
   (*
fun getBlock (a:int) (g:((int * (int list)) list)) = 
					if (List.length ((List.nth g (searchNode a g))#2)) > 1 then [] 					
					else ( ((List.nth g (searchNode a g))#1) :: (getBlock (List.nth ((List.nth g (searchNode a g))#2) 0) g) )
   |getBlock _ _ = [];
*)

fun isaleader [x] (l :: leads) = if( x = l) then true else isaleader [x] leads
   |isaleader _ _ = false;

fun getBlock [a] g lNodes= 
					let
					val index = searchNode a g;
					val nextnode = intG.succ a g;
					(*val fnode = List.nth nextnode 0;*)
					in
					if(((List.length nextnode ) > 1) orelse (isaleader nextnode lNodes)) then [a]
					else [a] @ getBlock nextnode g lNodes
					end
   |getBlock _ _ _ = [];

fun getBasicBlocks (x :: leads) lNodes graph = [getBlock [x] graph lNodes] @ getBasicBlocks leads lNodes graph
   |getBasicBlocks _ _ _ = [];
   
fun basicBlocks start graph = let
					val leaderNodes = refineList ([start] @ (extractLeadsSucc graph) @ (extractLeadsPred graph graph));
					in
					getBasicBlocks leaderNodes leaderNodes graph
					end;

fun getstart (x :: xs) (s :: ss)  g= if ((List.length (intG.pred x g)) = 0) then (x) 
                                else ( if (x = s) then (x) else getstart (intG.pred x g) (s :: ss) g )
   |getstart (x :: xs) []  g = x;

fun basicBlockList ((x, y) :: xs) = 
                let
                val preds = intG.pred x ((x, y) :: xs);
                val start = getstart (intG.nodes ((x, y) :: xs)) preds ((x, y) :: xs);
                in
                basicBlocks start ((x, y) :: xs)
                end;
(*
structure bblockType:GraphType = 
struct
    type nType = int list;
end;

structure bBlockGraph = dirgraph (bblockType);

*)




