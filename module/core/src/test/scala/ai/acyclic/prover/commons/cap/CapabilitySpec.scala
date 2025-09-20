package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.cap.Capability.<>
import ai.acyclic.prover.commons.testlib.BaseSpec

class CapabilitySpec extends BaseSpec {
  import CapabilitySpec.*

  it("can enable capability in compile-time & runtime") {

    assert(ex1 == ex0)
  }

  describe("can revoke all capabilities") {

    it("by upcasting") {

      ex1: Ex
      ex12: Ex
      ex12: Ex <> Cap1
      ex12: Ex <> Cap2

      ex12: (Ex <> Cap2) <> Cap1
      ex123: ((Ex <> Cap3) <> Cap2) <> Cap1
      ex123: ((Ex <> Cap2) <> Cap3) <> Cap1
    }

    it("by function") {

      {
        val revoked = Capability.revokeAll(ex1)
        assert(revoked == ex1)
        shouldNotCompile("revoked: Ex ^: Subject.Cap1")
      }

      {
        val revoked = Capability.revokeAll(ex12) // (Capability.revoke.chain)
        // buggy compiler! circumventing

        assert(revoked == ex12)
        shouldNotCompile("revoked: Ex ^: Subject.Cap1")
      }
    }
  }

  it("equivalence of type regardless of being left/right associative") {

    implicitly[((Ex <> Cap1) <> Cap2) =:= ((Ex <> Cap1) <> Cap2)]

    ex12
//    val d2: (Ex <> Cap1) <> Cap2 = d1
  }

  it("can swap capabilities") {

    val ex21: (Ex <> Cap2) <> Cap1 = ex12

    assert(ex21 == ex12)
  }

  describe("can revoke some capability") {

    it("by upcasting") {

      {
        val revoked: Ex <> Cap1 = ex12
        assert(revoked == ex12)
        shouldNotCompile("revoked: Ex ^: Subject.Cap2")
      }

      {
        val revoked: Ex <> Cap2 = ex12
        assert(revoked == ex12)
        shouldNotCompile("revoked: Ex ^: Subject.Cap1")
      }
    }

    it("by function") {

      {
        object revoke extends Capability.revoke[Cap1]

        val revoked = revoke(ex12)(revoke.last[Ex <> Cap2, Cap1]) // fuck scala
        assert(revoked == ex12)
        val _: Ex <> Cap2 = revoked
        shouldNotCompile("revoked: Ex <> Subject.Cap1")
      }

      {
        val revoke = Capability.revoke[Cap2]
        val revoked = revoke(ex12)(revoke.last) // fuck scala
        assert(revoked == ex12)
        val _: Ex <> Cap1 = revoked
        shouldNotCompile("revoked: Ex <> Subject.Cap2")
      }
    }
  }

}

object CapabilitySpec {

  trait Cap1 extends Capability {}

  trait Cap2 extends Capability {}

  trait Cap3 extends Capability {}

  trait Ex {

    def fn(v: Int): Int
  }

//  trait Ext[C <: Subject.Capability] extends Ex {}

  val ex0: Ex = { v => v + 1 }

  val ex1: Ex <> Cap1 = Capability(ex0).<>[Cap1]

  val ex12: (Ex <> Cap1) <> Cap2 = Capability(ex1).<>[Cap2]

  val ex123: ((Ex <> Cap1) <> Cap2) <> Cap3 = Capability(ex12).<>[Cap3]
}
