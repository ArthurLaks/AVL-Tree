This repository represents my attempt to learn Common Lisp.  
The most substantial programs in the repository are the binary search
trees.BST.lisp is an implementation of the binary search tree constructed 
entirely from cons cells.  This is how to make a node is constructed:

 
    (cons node-value (cons right-node left-node))

Thus, I did not define any new types.  AVLBST.lisp is an AVL tree that is built on BST.lisp.  Its nodes are like this: 


    (cons (cons node-value depth) (cons left-node right-node))

The rest of the programs are simple functions that I made in order to help
learn the language.  All of the programs simply define functions.  I divided 
them into two categries: those that use recursion and those that use iteration.
The only program that does not fit into this system of classification is Point.lisp,
an attempt to implement encapsulation through closures.
Note that all of the programs in the repository simply define functions. 
They can only be used by loading them into the REPL.
