package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.PreDef._

trait FnFixture {

  lazy val _fn0: Int :=> Int = Impl { v =>
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
