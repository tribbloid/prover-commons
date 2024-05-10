package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.hom.HomSystem

trait ChainSelf extends Fns {

  lazy val chainSelf: HomSystem.AndThen[fn0.In, fn0.Out, Int] = {

    val repr: HomSystem.Fn.AsRepr[fn0.In, fn0.Out, fn0.type] = HomSystem._fnAsRepr(fn0)

    val s1 = fn0.andThen[Int](
//      fn0
      repr
    )

    val s2 = {
      fn0.^(fn0)
    }

    s1
  }

}
