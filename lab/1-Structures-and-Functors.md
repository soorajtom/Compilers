# Structures and Functors.

When writing large programs it is necessary to organise the code into
small units or modules. ML's structures and functors are the way to
modularise code. The programming interface to the lexers and parsers
generated using mllex and mlyacc uses structure and a basic
understanding of structures is required.

Here is a quick guide for SML structures.


## Assignment 1: The Tree module.


Consider the following signature for ordered element

```
signature ORD =
sig

type t

val le : t -> t -> bool

end

```

1. Write a structure IntOrd of signature ORD that captures the
   ordering among elements.

2. Define a generic QSort functor that takes a structure with
   signature ORD and generates a module with a single quick sort
   function.
