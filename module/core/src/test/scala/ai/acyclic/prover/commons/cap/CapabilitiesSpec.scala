package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.cap.Capability.>>
import ai.acyclic.prover.commons.testlib.BaseSpec
import shapeless.test.illTyped

class CapabilitiesSpec extends BaseSpec {
  import CapabilitiesSpec._

  it("can enable capability in compile-time & runtime") {

    assert(ex1 == ex0)
  }

  describe("can revoke all capabilities") {

    it("by upcasting") {

      val _: Ex = ex1
      val _: Ex = ex12
      val _: Ex >> Cap1 = ex12
      val _: Ex >> Cap2 = ex12

      // TODO: how do the following even work?
      val _: (Ex >> Cap2) >> Cap1 = ex12
      val _: ((Ex >> Cap3) >> Cap2) >> Cap1 = ex123
      val _: ((Ex >> Cap2) >> Cap3) >> Cap1 = ex123
    }

    it("by function") {

      {
        val revoked = revokeAll(ex1)
        assert(revoked == ex1)
        illTyped("revoked: Ex ^: Subject.Cap1")
      }

      {
        val revoked = revokeAll(ex12)
        assert(revoked == ex12)
        illTyped("revoked: Ex ^: Subject.Cap1")
      }
    }
  }

  describe("can revoke some capability") {

    it("by upcasting") {

      val revoked: Ex >> Cap1 = ex12
      assert(revoked == ex12)
      illTyped("revoked: Ex ^: Subject.Cap2")
    }

    it("by function") {

      val revoked = (new Cap1 {}).revoke(ex12)
      assert(revoked == ex12)
      val _: (Ex >> Cap2) = revoked
      illTyped("revoked: Ex ^: Subject.Cap1")
    }
  }

}

object CapabilitiesSpec extends Capabilities {

  trait Cap1 extends Capability {}

  trait Cap2 extends Capability {}

  trait Cap3 extends Capability {}

  trait Ex {

    def fn(v: Int): Int
  }

//  trait Ext[C <: Subject.Capability] extends Ex {}

  val ex0: Ex = { v => v + 1 }

  val ex1: Ex >> Cap1 = ex0 >>: new Cap1 {}

  val ex12: (Ex >> Cap1) >> Cap2 = ex1 >>: new Cap2 {}

  val ex123: ((Ex >> Cap1) >> Cap2) >> Cap3 = ex12 >>: new Cap3 {}
}
