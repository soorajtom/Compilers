# Stand alone compilation.

The sample code here demonstrates the following.

1. A program which can easily be loaded by sml-nj interactively using
   the command `sml hello.cm`

2. A program that can be easily compiled into a standalone executable
   using mlton.


The strategy is the following

1. Structure the project like you would with sml-nj with a .cm file
   and other stuff.

2. Add a wrapper main.sml which calls the appropriate functions. This
   is only for mlton.

3. Have a hello.mlb which is use to compile for mlton using the `mlton
   hello.mlb`


## Rational for this structure

SML/NJ provides a nice interactive environment while developing,
however getting a standalone is (1) a pain and (2) the executable is
not very efficient. Mlton is meant for standalone compilation and the
executable is very performant. However, compilation takes a lot of
time and space. Moreover, it does not have an interactive interpreter
for quick development.

## Standalone executable with SML/NJ

You can create a heap image with ml-build and later on load
it. However, this is still interpreted and beware of the change of
command line arguments (the prgram name will be your smlnj iterpreter. See
the companion build.sh script for how this is done.
