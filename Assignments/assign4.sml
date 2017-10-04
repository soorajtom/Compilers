


signature Graph = sig
   eqtype node
   type   graph
   
   val empty : graph
   (*
   val empty   : graph
   val newNode : graph       -> node
   val addEdge : (node,node) -> graph -> ()
   val nodes   : graph       -> node list
   val suc     : graph       -> node list
   val pred    : graph       -> node list
*)
end;

signature GraphType = sig
    
    eqtype nodet
    type grapht
end;


functor dirgraph( types:GraphType ):Graph = 
struct
    
    type node = types.nodet
    type graph = types.grapht
    
    val empty =    
end;
