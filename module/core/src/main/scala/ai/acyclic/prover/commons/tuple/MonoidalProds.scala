package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.tuple.HLists.{*:, _1}

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
trait MonoidalProds extends MonoidalProds.RightAssociative with HListConverterMixin with FlatReprMixin {
  self: Singleton =>

  def cons[L <: VBound, TAIL <: Prod](head: Element[L], tail: TAIL): L ><: TAIL
  def deCons[L <: VBound, TAIL <: Prod](cons: L ><: TAIL): (Element[L], TAIL)

  sealed trait _TupleOps[SELF <: Prod] {

    def self: SELF

    def ><:[
        L <: VBound
    ](
        head: Element[L]
    ): L ><: SELF = cons(head, self)
  }

  implicit class tupleOps[SELF <: Prod](val self: SELF) extends _TupleOps[SELF] {}

  implicit def eyeExtension(s: this.type): tupleOps[Eye] = tupleOps[Eye](Eye)
}

object MonoidalProds {

  import scala.language.implicitConversions

  /**
    * Cartesian product on its own (armed with product & left/right projection) is not monoidal:
    *
    * (X >< Y) >< Z and X >< (Y >< Z) are different types
    */
  sealed trait Cartesian {

    type VBound // TODO: remove, should be superseded by <:< in Element

    type Prod

    protected val _1: Prod

    type Element[T <: VBound]

    /**
      * Identity element of the product (MATLAB terminology)
      */
    final def Eye: Eye = _1
    type Eye = _1.type

    type Nil = Eye
    val Nil: Nil = Eye
  }

  trait LeftAssociative extends Cartesian {

    infix type :><[+TAIL <: Prod, +R <: VBound] <: Prod
  }

  /**
    *   - head to the left
    *   - tail to the right
    */
  trait RightAssociative extends Cartesian {

    /**
      * The product (Bra-ket notation)
      */
    infix type ><:[+L <: VBound, +TAIL <: Prod] <: Prod

  }
}
