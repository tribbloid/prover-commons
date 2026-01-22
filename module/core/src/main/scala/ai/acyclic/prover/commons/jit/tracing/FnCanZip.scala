package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.jit.hom.Hom.Fn

trait FnCanZip {

  /**
    * this is a typeclass that can recursively zip the input and output of multiple [[Fn]] into one [[Fn]].
    *
    * e.g. given (Fn[I1, O1], Fn[I2, O2], Fn[I3, O3]), the typeclass should produce Fn[(I1, I2, I3), (I1, I2, I3)]
    */
  trait FnZippable[I] {
// TODO: can you implement this?
    type Out <: Fn[?, ?]
  }

  object FnZippable {}
}
