
### TODO for prover-commons graph integration

- the largest missing component is ranked graph (RG) as DSL element of composition, which is a graph with "distinguished" head/tail nodes
- RG can be implicitly casted from any node OR value as a singleton
- 2 basic operands: union & composition (which is union + extra edges)
- extended operands:
- replicate (by shuffling IDs but keep values)
- cartesian composition
- rebase (replicate + )

Merging/deconflicting algorithm is necessary for prover-commons, as a cycle doesn't always circle back to the same value, the new value needs to be merged back to the old value (or fail sanity check)

After that, the easiest solution is always swapping out the graph backend of DSL Layout (instead of re-implementing them from scratch), it is unknown if it is easy.

