# Compiling to Reverse Polish notation (RPN)


This directory illustrates a program to convert converts from integer
expressions to reverse polish notation. We have structured the program
like a classical compiler where the source language is normal
expressions and the target language is a set of instructions for a
reverse polish calculator.

## Test machine

This directory also contains the source for a standalone reverse
polish calculator rp. This can be used to test the compiler. The
following input can be give to the calculator.

1. A number with an optional sign [+-~]. One can use the ~ sign for
   negation following the Standard ML convention

2. Single character commands `p` (for printing the top of the stack)
   `s` for printing the entire stack and `c` for clearing the stack.

3. Line comments starting with the character `#`.

```
# This is a sample script for rpn
# We compute the answer to life universe and everything

2 40 +ps

```

The above program prints 42 followed by [42].

## Building the RPN machine.

The program `rp` is a stand alone program which can be compiled using
the mlton compiler. One just needs to do a make

```
make

```
