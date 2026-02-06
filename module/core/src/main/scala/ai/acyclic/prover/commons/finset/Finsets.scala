package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.finset.Tuples.{:*, _0}

/**
  * following convention of lean4 prover, Finsets.Fin represents a finite set of heterogeneous elements, each bounded by
  * VBound.
  *
  * it was previously called "Tuple", this was a mistake
  */
trait Finsets extends LeftNested with FromTupleMixin with ToTupleMixin {}
