(** * A tutorial to get started with Standard ML.


This is a tutorial to get you started with standard ml. You should
have with you all the tools required for the compiler course. See the
wiki section on getting started.

https://bitbucket.org/piyush-kurur/compilers/wiki/Getting%20Started

By now you would have realised that anything that starts with "(*" and
ends with "*)" is a comment. Fortunately, these comments nest unlike the
case of /* */ in C.

So we have our first lesson which is to write comments.

*)


(*

SML programs are usually divided into modules which are called
structures. For this Tutorial this is not very relevant. But it is
quite useful when you want to build large programs as will be the case
with compilers. So for time being ignore the next line and only concentrate
on the contents withing the struct ... end.

*)

structure Tutorial =
struct


(* Values and their types *)

val aString     = "Hello  world"
val aChar       = #"c"
val aBool       = true
val anotherBool = false
val anInt       = 42
val aReal       = 42.0
val aList       = [1,2,3]

val aPair       = ("the answer to life, universe, and everything", 42)
val aTriple     = (1, 2.4, "hello")
val aUnit       = () (* empty product type *)

(*

SML is strongly typed unlike say ruby or python. However, one does not
need to explicitly give the type declaration. The compiler "infers"
the type using the context. This is one of the powerful features that
makes programming in ML a pleasant experience.

Exercise: Enter the above definitions on the sml interpreter and
note the types that the interpreter prints.

*)


(*

We can also provide the type as the next example shows. In the
definition below the origin : real * real asserts that origin is a
constant of type real * real.

*)

val origin : real * real = (0.0, 0.0)

 (* Exercise: What happens with the following definition ? *)
 (* val origin : real * real = (0.0, "0.0") *)



 (* We can give new names to types. This is called type aliasing  *)
type vector2d = real * real

val unitx : vector2d = (1.0,0.0)          (* unit vector along x-axis *)
val unity : vector2d = (0.0,1.0)          (* unit vector along y-axis *)





(*

** Functions.

Functions are what makes programming in sml great. Let us start by
defining the simplest factorial function. Since function application
is a very common operation, they have a very simple syntax: applying f
on x is just f x

*)

fun fact n = if n <= 0 then 1
	     else n * fact (n - 1) (* Notice the recursive use of fact *)

(** The idea of currying.

In ML we only have functions with one arguments. To illustrate let us
look at the following variants of add.

*)

fun add (u,v) = u + v
fun addp u v  = u + v

(*

Both the functions only take one argument each:

val add : int * int -> int

val addp : int -> (int -> int)

The first variant takes a parameter of type (int * int), i.e. a tuple
and returns an int.  The second variant takes a single int parameter
and returns a function that takes a single int parameter and returns
int.


Functional programmers call the variant add as the uncurried version
and addp as the curried version.


The advantage of the curried version is that we can "partially" apply
addition.

*)

val increment = addp 1

(*

In general a function that takes n parameters of types t1, t2 .. tn
and returns a value t can be thought of as the type t1 -> (t2 ->
(.. -> tn)). This is called currying. Currying is a powerful idea that
allows such partial applications.


We can convert from one curried version to its uncurried version and
vice-versa as follows.


*)


fun curry   f  x y  = f (x,y)	(* convert to curried form   *)
fun uncurry f (x,y) = f x y    (* convert to uncurried form *)


val addp1 = curry  add           (* Notice the use of val instead of fun *)
val incr  = addp1 1
val add1  = uncurry addp


(*

The functions above illustrate an important aspect of Sml, Functions
are truly first class as they can be passed to other functions or
stored in a data structure like list

*)

val someIntFunctions = [addp 1, addp 2, addp 3]

(** * List functions and pattern matching

In ML lists are written as follows

*)

val firstFewOddPrimes = [3,5,7]


(*

The x :: xs denotes a list with x as its first element and xs as the
rest of the list

*)

val firstFewPrimes = 2 :: firstFewOddPrimes


(*

Here is an example of a list function that applies a given function to
all the elements on the list

*)

fun map f []         = []
  | map f (x :: xs)  = f x :: map f xs


(*

Let us see what happens if we increment the primes: Silly program
but illustrates the use of map and partial application.

*)

val useless = map incr firstFewPrimes

val somemorestuff = map (addp 42) firstFewPrimes (* See the use of currying *)


(* Another function that is very useful  for processing lists are folds

fold f a [b0, b1 , b2, b3 ...] = f (f a b0) b1 ....

Think of op as an operator we have

fold op a [b0,b1,b2 ...] = ((a op b0) op b1) op b2 ...

corresponds to the library function foldl

*)


fun fold _ x []      = x
  | fold f x (y::ys) = fold f (f x y) ys


(* Let us write a function to sum up a list of numbers. Notice that this is just a fold *)

val sum = fold addp 0  (* sum [x1,x2,x3..] = ((0+x1) + x2) ....) *)


(*

Let us write the product function which takes a product of the
list. Again it turns out to be just a fold

*)

val prod = let fun mul x y = x * y
	   in fold mul 1
	   end

(*

 We can use this to define factorial for which we first define the
 enumerate function

*)

fun enum a b = if a <= b
	       then a :: enum (a + 1) b
               else []

fun factorial n = prod (enum 1 n)

val fct = prod o enum 1  (* using function composition o *)

(*

Let us end by writing a main function that is the entry point for our
tutorial.

*)

fun main _ = let val (question,answer) = aPair
	     in map print [question, ": ", (Int.toString answer), "\n"]; 0 end


end


(*

Exercise: Our "folding" is from left, we could define a right fold as
well which does the folding from right

i.e  fold op [a,b,c,...z] b =  a op (b op ... (y op (z op b)))


If the operator o is not commutative this can be different.

The standard library has its own variants of foldr and foldl but they
accept the functions in uncurried form. Have a look at their types.


*)
