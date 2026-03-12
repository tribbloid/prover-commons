package ai.acyclic.prover.commons.jit.fixture

import ai.acyclic.prover.commons.jit.Hom.{Const, Fn}

object ConstLike {

  val s1 = Const.Provided[Int](1)

  val s2 = Const.Provided[String]("a")
}
