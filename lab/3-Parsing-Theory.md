# Parsing theory (Deadline: September 22, 2017).

In the class, we studied the theory behind writing parsers for
programming language. This lab-session is aimed at consolidating that
understanding using some programming assignments. This will be a
graded assignment and we will set the due date soon. For all the tasks
given below, the input is a context free grammar given in some
suitable form. I would suggest that you define a ML data type to
capture context free grammars.


1. Formulate an ML data type to capture context free grammars. In the
   rest of the steps you will write functions to process this data
   type. This will reduce the burden of writing I/O routines for entering
   the grammar.


2. Write an ML function to compute the FIRST and FOLLOW of all
   non-terminals in your grammar. Implicit in this problem is to
   design an ML data type for storing this table.

3. Using the output of step 1, compute the LL(1) parsing table for the
   grammar (or report not in LL(1)).

4. Use 2 in generating SLR(1) tables and LR(1) tables. Report
   shift-reduce conflicts if the CFG cannot be handled by the SLR(1)
   (or LR(1)).

5. Use the above programs to analyse some of the grammars that we saw
   in the class (for example, expression grammar).

## Some helpful suggestions.

A data structure that comes handy in such context is what is known as
a Map (or a dictionary) from terminals to the associated list of
RHS's. See the [SML/NJ library][smlnj-lib].


[smlnj-lib]: <https://www.classes.cs.uchicago.edu/archive/2015/spring/22620-1/smlnj-lib.html>
