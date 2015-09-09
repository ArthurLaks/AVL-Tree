This repository contains implementations of the Binary Aearch tree and AVL tree
data structures.

BST.lisp is an implementation of the binary search tree constructed 
entirely from cons cells.  This is how to make a node is constructed:

 ````lisp
    (cons node-value (cons right-node left-node))
````

Thus, I did not define any new types.  AVLBST.lisp is an AVL tree that 
is built on BST.lisp.  Its nodes are defined like this: 

````lisp
    (cons (cons node-value depth) (cons left-node right-node))
````

Those trees are completely immutable.  That is, all of the functions defined in those
files take a tree as the first argument and return a new tree, leaving the argument untouched.
The new and old trees will share all of the nodes that were not affected by the modification.

The files in this repository define functions that act on trees and are not self-contained
programs.  They can only be tested by loading them onto the REPL or into another file.

