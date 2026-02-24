package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.compat.{*:, TupleX}
import ai.acyclic.prover.commons.util.Phantom

object Schemata {

  /**
    * Cartesian product on its own (armed with product & left/right projection) is not monoidal:
    *
    * (X >< Y) >< Z and X >< (Y >< Z) are different types
    */
  trait Cartesian {
    // TODO: VBound should fold into Element[T]:
    //  e.g. Element[T <: Int] is equivalent to Element[T] <: { def bound: T <:< Int }
    type VBound
    type Element[T <: VBound]

    type Prod

    /**
      * Identity element of the product (MATLAB terminology)
      */
    type Eye <: Prod
    type T0 = Eye // aliases
    type Nil = Eye
  }

  trait LeftAssociative extends Cartesian {

    infix type :><[TAIL <: Prod, R <: VBound] <: Prod
    type T1[R <: VBound] = Eye :>< R

    type ><[X <: VBound, Y <: VBound] = Eye :>< X :>< Y
  }

  /**
    *   - head to the left
    *   - tail to the right
    */
  trait RightAssociative extends Cartesian {

    /**
      * The product (Bra-ket notation)
      */
    infix type ><:[L <: VBound, TAIL <: Prod] <: Prod
    type T1[L <: VBound] = L ><: Eye

    type ><[X <: VBound, Y <: VBound] = X ><: Y ><: Eye
  }

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
  trait Monoidal extends Schemata.RightAssociative {

    trait Backbone extends Schemata.Monoidal {

      override type VBound = Monoidal.this.VBound
    }

    object SchemaMixin extends Backbone {

      override type Element[V <: VBound] = Unit

      trait Prod extends Serializable with Phantom {

        type Header <: TupleX.Prod
      }

      trait Eye extends Prod {

        override type Header = TupleX.Eye
      }

      trait ><:[
          L <: VBound,
          +TAIL <: Prod
      ] extends Prod {

        val tail: TAIL

        override type Header = L *: tail.Header
      }
    }
  }

}
