(*Author : Sooraj Tom, 111501036
*)

use "assign4.sml";

signature Instruction =
sig
    eqtype inst
    val useSet : inst -> AtomSet.set (* Get the use set of an instruction *)
    val defSet : inst -> AtomSet.set (* Get the def set of an instruction *)
end

type insType = int;

structure insGraphtype:GraphType =
struct
    type nType = insType;
end

structure BBGraphType:GraphType =
struct
    type nType = insType list;
end

structure insGraph = dirgraph(insGraphtype);
structure BBGraph = dirgraph(BBGraphType);
val sampleg:insGraph.graph = [(1,[2]), (2,[3]), (3,[4]), (4,[5]), (5,[6, 2]), (6,[])];

fun connectcomp (x :: xs) bblist bbgraph=
    let
        val succlist = insGraph.succ (List.last x);
    in
        0
    end;
fun addtograph (x :: xs) graph =
    BBGraph.newNode x (addtograph xs graph)
   |addtograph [] graph = BBGraph.empty;

fun makebbgraph start dfgraph =
    let
        val bblist = basicBlocks start dfgraph;
        val bbg = addtograph bblist;
    in
        connectcomp bblist bblist (bbg)
    end;

structure SetMapKey:ORD_KEY =
struct
    type ord_key = insType;
    val compare = Int.compare;
end

structure ASetMap = ListMapFn(SetMapKey);

type ASetMapType = AtomRedBlackSet.set ASetMap.map;

fun makeASetMap (x :: xs) = ASetMap.insert ((makeASetMap xs), x, AtomSet.empty)
   |makeASetMap [] = ASetMap.empty;

structure Inst:Instruction =
struct
    type inst = insType;
    fun useSet x = AtomSet.fromList (List.map Atom.atom (List.nth ([[],["a"],["b", "c"],["b"],["a"],["c"]], x - 1)));
    fun defSet x = AtomSet.fromList (List.map Atom.atom (List.nth ([["a"],["b"],["c"],["a"],[],[]], x - 1)));
end;

fun findGenKillbb (x :: blist)=
   let
       val g1 = Inst.useSet x;
       val k1 = Inst.defSet x;
       val (g2, k2) = findGenKillbb blist;
       val newgen = AtomSet.union (g1, (AtomSet.difference (g2, k1)));
       val newkill = AtomSet.union (k1, k2);
   in
       (newgen, newkill)
   end
  |findGenKillbb [] = (AtomSet.empty, AtomSet.empty);

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

fun equatemaps map1 map2 =
    let
        val Atomsetlist = ASetMap.listItems map1;
        val Atomlist = List.concat (List.map AtomSet.listItems Atomsetlist);
        val Mapstr1 = List.map (Atom.toString) Atomlist;
        val Atomsetlist = ASetMap.listItems map2;
        val Atomlist = List.concat (List.map AtomSet.listItems Atomsetlist);
        val Mapstr2 = List.map (Atom.toString) Atomlist;
    in
        Mapstr2 = Mapstr1
    end;

fun getInOut dfgraph inmap outmap =
        let
            val insNodes = insGraph.nodes dfgraph;
            val p_inmap = getInInst insNodes inmap outmap;
            val p_outmap = getOutInst insNodes inmap outmap dfgraph;
        in
            if((equatemaps inmap p_inmap) andalso (equatemaps outmap p_outmap))then
            (inmap, outmap)
            else
            (getInOut dfgraph p_inmap p_outmap)
        end;

fun display x =
        List.map (List.map (Atom.toString)) (List.map (AtomSet.listItems) (ASetMap.listItems x));

fun question1 dfgraph =
    let
        val (inmap, outmap) = getInOut dfgraph (makeASetMap (insGraph.nodes dfgraph)) (makeASetMap (insGraph.nodes dfgraph));
    in
        (*(List.map (AtomSet.listItems) (ASetMap.listItems inmap), List.map (AtomSet.listItems) (ASetMap.listItems outmap))*)
        (inmap, outmap, display inmap, display outmap)
    end;

val answer1 = question1 sampleg;



(*fun getGenKillbb (x :: bblock) = findGenKillbb (bblock) (Inst.defSet x) (Inst.useSet x)
   |getGenKillbb _ = (AtomSet.empty, AtomSet.empty);

fun GenKillbb bblock = getGenKillbb (List.rev bblock);*)

val res = findGenKillbb [2,3,4,5];
List.map (Atom.toString) (AtomSet.listItems (#1res));
List.map (Atom.toString) (AtomSet.listItems (#2res));
