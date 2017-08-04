# Familiarising with the course lab.

This is the first session for the course lab. This will involve mostly
getting familiar with course management system and bitbucket. We will
also do a bit of Standard ML programming.

## Getting started.

First create an account on bitbucket. Then fork the compilers
repository into your account. We now want a local clone of the
repository.


```
git clone git@bitbucket.org:yourusername/compilers.git
cd compilers
git remote add upstream https://bitbucket.org/piyush-kurur/compilers.git
git remote -v # should show you two remotes
              # origin which is git@bitbucket.org:yourusername/compiler.git
			  # upstream which is https://bitbucket.org/piyush-kurur/compilers.git
```


More details available at https://confluence.atlassian.com/get-started-with-bitbucket/

## Tutorial in Standard ML

Now inspect the file tutorial.sml in
`programming-in-ml/getting-started/`. Here is a sample interaction.

```
sml  # start the sml interpreter.
Standard ML of New Jersey v110.79 [built: Mon Jul  3 21:22:56 2017]
- val answer = 42;
val answer = 42
- answer - 2;
val it = 40 : int

```

## Assignment 0.

Write SML functions/datatypes for the following tasks.

1. Define a data type for binary tree.
```
datatype 'a Tree = ...

```
2. Write a function `inorder` that lists the tree in inorder.

```
val inorder : 'a Tree -> 'a list
```
3. Write a function to rotate a tree anti-clockwise at the root.
```
val anticlockwise : 'a Tree -> 'a Tree
```
