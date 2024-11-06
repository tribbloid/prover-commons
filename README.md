# prover-commons

## pushing the boundary of Curry-Howard-Lambek trinity with Scala compiler

This is a lightweight gradle submodule (LGS), source code with such structure can either be build independently, or be attached to and published under an existing project.

At this moment, it is attached to:

- shapesafe
- spookystuff

See [LGS scaffold](https://github.com/tribbloid/scaffold-gradle-kts) for a minimally working example

The scopes of this projects are:

- providing abstractions for lazy, inductive graph, poset, semilattice, tree
  - and their visualisation utilities
- providing tools for type & logic traversing tools, including subtype semilattice, type constructor graph, implicit search & deduction tree
  
