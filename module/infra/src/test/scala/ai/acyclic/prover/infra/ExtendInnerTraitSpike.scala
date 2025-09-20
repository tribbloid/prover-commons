package ai.acyclic.prover.infra

//class ExtendInnerTraitSpike extends BaseSpec {}

object ExtendInnerTraitSpike {

  trait A {

    trait X
  }

  object A1 extends A
  object A2 extends A

//  trait B extends A1.X with A2.X {} // this won't work in both Scala 2 & 3
}
