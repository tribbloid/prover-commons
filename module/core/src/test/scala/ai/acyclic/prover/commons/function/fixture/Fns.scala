package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom._
import ai.acyclic.prover.commons.function.Hom

trait Fns {

  final lazy val fn0: Int :=> Int = Hom { v =>
    v + 1
  }

  final lazy val fn1: Int :=> Seq[Long] = Hom { v =>
    Seq(v, v + 1, v + 2)
  }

  final lazy val fn2: Long :=> Seq[Double] = Hom { v =>
    Seq(v, v * 0.1, v * 0.2)
  }

//  lazy val _fn0: Int :=> Int = {
//
//    val x: Int :=> Int = _fromVanillaBase { v =>
//      v + 1
//    }
//
//    val a: Int :=> Int = fromVanilla { v =>
//      v + 1
//    }
//
//    val b: Int :=> Int = Fn { v =>
//      v + 1
//    }
//    b
//  }

  lazy val fn0Text: String = {

    val result = fn0.explain.nodeText

    result
  }

}
