[![Build Status](https://secure.travis-ci.org/diagrams/dual-tree.png)](http://travis-ci.org/diagrams/dual-tree)

Rose (n-ary) trees with both upwards- (*i.e.* cached) and
downwards-traveling (*i.e.* accumulating) monoidal annotations.  This
is used as the core data structure underlying the
[diagrams framework](http://projects.haskell.org/diagrams), but
potentially has other applications as well.

Abstractly, a `DUALTree` is a rose (n-ary) tree with data of one type
at leaves, data of another type at internal nodes, and two types of
monoidal annotations, one travelling "up" the tree and
one traveling "down".

For a bit of background and motivation, see the paper 

[Brent Yorgey. *Monoids: Theme and Variations*.](http://www.cis.upenn.edu/~byorgey/pub/monoid-pearl.pdf) Haskell Symposium, 2012.

To install,

    cabal install dual-tree
