package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.api.{HasMorphism, HasPoly}

trait HomSystem_Imp0 extends HasMorphism with HasPoly with Serializable {

  final type IUB = Any

  implicit def FnAsFunction1[I, R](fn: FnCompat[I, R]): I => R = { i: I =>
    fn.apply(i)
  }

}
