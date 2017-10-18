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

signature Instruction =
sig
    eqtype inst
    val useSet : inst -> AtomSet.set (* Get the use set of an instruction *)
    val defSet : inst -> AtomSet.set (* Get the def set of an instruction *)
end

structure Inst:Instruction =
struct
    type inst = string;
    fun useSet _ = AtomSet.empty;
    fun defSet _ = AtomSet.empty;
end;

fun getInx x outmap =
        let
            val usex = (Inst.useSet x);
            val outx = AtomMap.lookup outmap x;
            val defx = (Inst.defSet x);
        in
            AtomSet.union (usex, (AtomSet.difference (outx, defx)))
        end

fun getInIns (x :: insNodes) inmap outmap = AtomMap.unionWith (AtomSet.union) ( AtomMap.insert inmap x (getInx x outmap) ) (getInIns insNodes inmap outmap)
   |getInIns [] _ _ = AtomMap.empty;

fun getInOut insNodes inmap outmap =
        let
            val p_inmap = getInIns insNodes inmap outmap;
            val p_outmap = getOutIns insNodes inmap outmap;
        in
            if((p_inmap = inmap) andalso (p_outmap = outmap))then
            (inmap, outmap)
            else
            getInOut insNodes p_inmap p_outmap
        end
