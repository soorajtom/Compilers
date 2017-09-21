# Assignment 2: Support Parenthesis in the expression.

The reverse-polish directory in this repository contains the source
code for a reverse polish calculator called `rp` and an expression
compiler `ec` that compiles a normal expression into reverse polish
notation.

1. Unfortunately `ec` does not support bracketing. So you
   cannot write `2 * (2 + 3)`.

2. Currently, the operator `*` has more precedence than `+`. Make it the other way
   and see how it works.

3. Look into the `expr.grm.desc` file for the LALR(1) parsing table.

4. See what happens if both `+` and `*` is given the equal precedence.


You can compile the ec and rp program using the make command

```
make all
```

If you want to recompile stuff you can try

```
make clean
make all
```
