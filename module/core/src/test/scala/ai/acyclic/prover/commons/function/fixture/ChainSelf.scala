package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.HomSystem
import ai.acyclic.prover.commons.function.hom.HomSystem.Fn

trait ChainSelf extends Fns {

  lazy val chainSelf: Fn.AndThen[fn0.In, fn0.Out, Int] = {

    val s1 = fn0.andThen[Int] {
      fn0

//      val repr = HomSystem.FnOps(fn0)
//      repr
    }

    val s2 = {
      fn0.^(fn0)
    }

    s1
  }

}
