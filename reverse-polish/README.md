# Compiling to Reverse Polish notation (RPN)


This directory illustrates a program to convert converts from integer
expressions to reverse polish notation. We have structured the program
like a classical compiler where the source language is normal
expressions and the target language is a set of instructions for a
reverse polish calculator.

## Quick start

This directory contains the source code for two standalone programs.

`ec`
:   The expression compiler which compiles normal expressions into
    reverse polish notation. You should think of the expression as
    your high level language and reverse polish notation as your low
    level language.

`rp`
:   The reverse polish machine, which interprets the expression in
    reverse polish notation. This is given to test your compiler.

You can build these programs using the following make command

```
make all
```

## The RPN machine.


The RPN takes a sequence of machine instructions which consists of

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

The above program prints 42 (because of the `p`) followed by `[42]`
(because of the `s`). To understand the operation of this machine try
running the reverse polish machine by typing.

```
./rp
```

You can use the command s (stack trace) to see how the machine
functions.

## Source code organisation of the compiler.

The compiler is essentially spread across three structures namely
`Ast`, `Machine`, and `Translate`. The are available in the files
`ast.sml`, `machine.sml`, and `translate.sml` respectively. Concentrate
on the following datatypes and values.

The data type `Ast.Expr`
:  Models the abstract syntax tree for the expression. This should be
   thought of as the high level language. The expression program is
   just `Ast.Expr list`

The data type `Machine.Inst`.
:    Models instructions to the RPN machine. A program to the RPN
	 machine modelled by `Machine.Program` is just `Machine.Inst list`.


The `Translate.compile`
:    This function compiles the high level language of expression program
     (modelled by `Ast.Expr list`) into a RPN-program (modelled by `Machine.Program`)


The parsing phase of the compiler is built using `mlyacc` and
`mllex`. Have a look at `expr.grm` and `expr.lex` for the grammar and
lexer definitions.
