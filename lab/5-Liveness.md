# Liveness analysis.

In this assignment we write generic code that will be used to perform
liveness analysis. The starting point is to abstract over
instructions. You can assume the following signature that abstracts
your instructions.


```
signature Instruction =
sig
    type inst
	val useSet : inst -> AtomSet.set (* Get the use set of an instruction *)
	val defSet : inst -> AtomSet.set (* Get the def set of an instruction *)
end

```

1. Given the representation of graphs as in the previous assignment,
   write functions to compute the in and out set of all the nodes in
   the graph.


2. Write a function that takes a list of instructions and computes the
   gen and kill set of the list. The list is supposed to capture a
   basic block with the first instruction as the entry to the basic
   block and the last instruction as the exit to the basic block

3. Use the function 2 and the basic block decomposition to compute the
   in and out set of your basic block. (Hint: You can use the same algorithm
   as in 1. Why is that?)
