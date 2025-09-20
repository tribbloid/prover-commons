package ai.acyclic.prover.infra

import ai.acyclic.prover.infra.testlib.BaseSpec

object IdHashSpike {

  case class V1(k: Long) extends AnyVal {

//    @transient lazy val k2 = k * 2 doesn't work

  }

}

class IdHashSpike extends BaseSpec {

  import IdHashSpike.*

  describe("identityHashCode") {

    it("for Long") {

      assert(
        System.identityHashCode(23L) ==
          System.identityHashCode(23L)
      )

      assert(
        System.identityHashCode(23L) !=
          System.identityHashCode(24L)
      )
    }

    it("for class") {

      val v1 = V1(23)
      val v2 = V1(23)

      v1.hashCode()
      assert((v1.hashCode()) == (v2.hashCode()))
      assert(System.identityHashCode(v1) != System.identityHashCode(v2))
    }

    it("for AnyRef") {

      val v1 = new AnyRef
      val v2 = new AnyRef

      assert(System.identityHashCode(v1) != System.identityHashCode(v2))
    }
  }
}
