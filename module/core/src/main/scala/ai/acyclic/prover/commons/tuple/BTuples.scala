package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.tuple.Tuples.{*:, _0}

import scala.language.implicitConversions

/**
  * bounded Tuples/HLists
  *
  * represents a finite ordered set of heterogeneous elements, each bounded by VBound.
  *
  * technically equivalent to a Tuple/HList >< verification evidence. Unfortunately this definition makes it very
  * difficult to define X >< (Y >< Z) from X and (Y >< Z), so such representation is abandoned. (It is possible albeit
  * with complex feature like type projection or match type)
  *
  * In addition:
  *
  *   - shapeless HList has an inefficient recursive memory layout in the heap
  *   - Scala 2 Tuple defines too many axioms
  *   - Scala 3 Tuple mixed the advantages of the above 2 but doesn't have a lot of functions (as in HListOps)
  *   - the most efficient implementation should have a contiguous, off-heap memory layout (See requirements in
  *     [[ai.acyclic.prover.commons.tuple.backbone.ContiguousMemoryBackbone]])
  *
  * consequently, this trait is only a scaffold, user should choose a backbone for exact implementation
  */
trait BTuples extends RightNestedAxiom with TupleConverterMixin with FlatReprMixin {
  self: Singleton =>

  def cons[HEAD <: VBound, TAIL <: Inductive](head: HEAD, tail: TAIL): HEAD ><: TAIL

  sealed trait _TupleOps[SELF <: Inductive] {

    def self: SELF

    def ><:[
        HEAD <: VBound
    ](
        head: HEAD
    ): HEAD ><: SELF = cons(head, self)
  }

  implicit class tupleOps[SELF <: Inductive](val self: SELF) extends _TupleOps[SELF] {}

  implicit def eyeExtension(s: this.type): tupleOps[Empty] = tupleOps[Empty](Empty)
}
