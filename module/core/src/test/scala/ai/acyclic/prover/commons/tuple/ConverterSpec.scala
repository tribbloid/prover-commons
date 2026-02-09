package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.tuple.backbone.RecursiveHeapBackbone

class ConverterSpec extends BaseSpec {

  object B1 extends RecursiveHeapBackbone {
    override type VBound = Any
  }

  object B2 extends RecursiveHeapBackbone {
    override type VBound = Any
  }

  object Conv extends Converter {
    override val from: B1.type = B1
    override val to: B2.type = B2
  }

  describe("Converter") {

    it("empty") {
      val v1 = B1.Empty
      val v2 = Conv(v1)
      assert(v2 == B2.Empty)
    }

    it("single") {
      //      import B1._
      //      import B1.tupleOps

      val v1 = 1 ><: B1.Empty
      val v2 = Conv(v1)
      assert(v2 == 1 ><: B2.Empty)
    }

    it("multiple") {

      val v1 = 1 ><: "a" ><: B1.Empty
      val v2 = Conv(v1)
      assert(v2 == 1 ><: "a" ><: B2.Empty)

    }
  }

  describe("Constrained") {

    object B3 extends RecursiveHeapBackbone {
      override type VBound = Int
    }

    object B4 extends RecursiveHeapBackbone {
      override type VBound = Int
    }

    object Conv2 extends Converter {
      override val from: B3.type = B3
      override val to: B4.type = B4
    }

    it("pass") {
      val v1 = 1 ><: B3.Empty
      val v2 = Conv2(v1)
      assert(v2 == 1 ><: B4.Empty)
    }

  }
}
