package ai.acyclic.prover.commons.compat

object RefinedProds {

  /**
    * this is the package supertyppe of monoidal structure, the schema is backed by a [[TupleX]] and a refinement which
    * contains both data and condition
    *
    * always right-associative to be compliant with Scala3 tuples (screw left-associative currying)
    */
  trait Monoidal {

    trait Prod {

      type Underlying <: TupleX
    }

    object _1 extends Prod {
      type Underlying = TupleX.Nil
    }

    trait ><[H, Tail <: Prod] {

      val tail: Prod

      type underlying = H
    }
  }
}
