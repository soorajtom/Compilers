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

(*type insType = string;*)

structure insGraphtype:GraphType =
struct
    type nType = string;
end

structure insGraph = dirgraph(insGraphtype);

structure SetMapKey:ORD_KEY =
struct
    type ord_key = string;
    val compare = String.compare;
end

structure ASetMap = ListMapFn(SetMapKey);

type ASetMapType = AtomRedBlackSet.set ASetMap.map;

fun makeASetMap (x :: xs) = ASetMap.insert ((makeASetMap xs), x, AtomSet.empty)
   |makeASetMap [] = ASetMap.empty;

structure Inst:Instruction =
struct
    type inst = string;
    fun useSet _ = AtomSet.empty;
    fun defSet _ = AtomSet.empty;
end;

fun getInx x outmap =
        let
            val usex = (Inst.useSet x);
            val outx = ASetMap.lookup (outmap, x);
            val defx = (Inst.defSet x);
        in
            AtomSet.union (usex, (AtomSet.difference (outx, defx)))
        end

fun getListIns (x :: succlist) inmap = (AtomSet.listItems (ASetMap.lookup (inmap, x))) @ getListIns succlist inmap
   |getListIns _ _ = [];

fun getOutx x inmap dfgraph =
        let
            val succlist = insGraph.succ x dfgraph;
        in
            AtomSet.fromList (getListIns succlist inmap)
        end

fun getInInst (x :: xs) inmap outmap =
                (ASetMap.unionWith (AtomSet.union)
                ((ASetMap.insert (inmap, x, (getInx x outmap))), (getInInst xs inmap outmap)))
   |getInInst _ _ _ = ASetMap.empty;

fun getOutInst (x :: xs) inmap outmap dfgraph=
                ASetMap.unionWith (AtomSet.union)
                ((ASetMap.insert (outmap, x, (getOutx x inmap dfgraph))), (getOutInst xs inmap outmap dfgraph))
   |getOutInst _ _ _ _= ASetMap.empty;

(*fun getSetMapList setMap = List.map (AtomSet.listItems) (ASetMap.listItems setMap)*)
fun equateinout tup1 tup2= true;

fun updateInOut (dfgraph, inmap, outmap) =
        let
            val insNodes = insGraph.nodes dfgraph;
            val p_outmap = getOutInst insNodes inmap outmap dfgraph;
            val p_inmap = getInInst insNodes inmap outmap;
        in
            (inmap, outmap)
        end;

fun getInOut dfgraph =
    let
        val insNodes = insGraph.nodes dfgraph;
        val inmap = ref (makeASetMap insNodes);
        val outmap = ref (makeASetMap insNodes);
    in
        while(equateinout (!inmap, !outmap) (updateInOut dfgraph (!inmap) (!outmap)))do(
            inmap := getInInst insNodes !inmap !outmap;
            outmap := getOutInst insNodes !inmap !outmap dfgraph
        );
        (inmap, outmap)
    end;
(*fun getInOut dfgraph inmap outmap =
        let
            val insNodes = insGraph.nodes dfgraph;
            val p_inmap = getInInst insNodes inmap outmap;
            val p_outmap = getOutInst (insNodes, outmap, inmap, dfgraph);
        in
            if(((getSetMapList p_inmap) = (getSetMapList inmap)) andalso
            ((getSetMapList p_outmap) = (getSetMapList outmap)))then
            (inmap, outmap)
            else
            (getInOut dfgraph p_inmap p_outmap)
        end;*)



(*fun getInOut insNodes dfgraph p_inmap p_outmap p_inmap p_outmap=
        (p_inmap, p_outmap)
   |getInOut insNode dfgraph inmap outmap p_inmap p_outmap=
        let
            val n_inmap = getInInst insNodes inmap outmap;
            val n_outmap = getOutInst (insNodes, outmap, inmap, dfgraph);
        in
            getInOut insNode dfgraph n_inmap n_outmap inmap outmap
        end;*)
