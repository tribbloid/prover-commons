package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec

class CapabilitiesSpec extends BaseSpec {
  import CapabilitiesSpec._

  it("can enable capability by runtime mixin") {
    trait Ex1 extends Ex
    val ex1: Ex1 = { v => v + 1 }

    val ex2 = ex1.enable[Subject.Cap1]

    assert(ex2 == ex1)
  }

  it("can disable capability by runtime cast") {
    trait Ex1 extends Ex with Subject.Cap1
    val ex1: Ex1 = { v => v + 1 }

    val ex2 = ex1.asInstanceOf[Ex]

    assert(ex2 == ex1)
  }

  ignore("type mixin can be used for SAM definition") {

    // none of the following works
//    type Ex1 = Ex with Subject.HasCap[Subject.Cap1]
//    val ex1: Ex1 = { v: Int => v + 1 } // not working
//
//    type Ext1 = Ext[Subject.Cap1] with Ext[Subject.Cap2]
//    val ext1: Ext1 = { v: Int => v + 1 } // not working
//
//    val ext2 = new Ext1 {
//      override def fn(v: Int): Int = v + 1
//    }
  }
}

object CapabilitiesSpec {

  object Subject extends Capabilities {

    trait Cap1 extends Cap {}

    trait Cap2 extends Cap1 {}
  }

  trait Ex extends Subject.NoCap {

    def fn(v: Int): Int
  }

  trait Ext[C <: Subject.Cap] extends Ex with Subject.NoCap {}

}
