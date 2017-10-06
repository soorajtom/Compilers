# Basic blocks (Deadline October 6, 2017).

1. Formulate an ML data type to capture directed graphs required to
   implement dataflow analysis for the graph. You can start with trying to
   implement a structure with the following signature.

         signature Graph = sig
           eqtype node
           type   graph     (* mutable variant of a graph *)

           val newNode : graph       -> node
           val addEdge : (node,node) -> graph -> ()
           val nodes   : graph       -> node list
           val suc     : graph       -> node list
           val pred    : graph       -> node list

        end


2. Write a general algorithm for computing the basic blocks in a
   directed graph with the above signature.


[smlnj-lib]: <https://www.classes.cs.uchicago.edu/archive/2015/spring/22620-1/smlnj-lib.html>
