package ai.acyclic.prover.commons.function.fixture

import ai.acyclic.prover.commons.function.Hom._
import ai.acyclic.prover.commons.function.Impl

trait Fns {

  lazy val fn0: Int :=> Int = Impl { v =>
    v + 1
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

}
