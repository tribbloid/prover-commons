package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.tuple.backbone.InductiveBackbone

class ConverterSpec extends BaseSpec {

  object B1 extends InductiveBackbone {
    override type VBound = Any
    override type Element[V <: VBound] = V
  }

  object B2 extends InductiveBackbone {
    override type VBound = Any
    override type Element[V <: VBound] = V
  }

  object Conv extends Converter {
    override val from: B1.type = B1
    override val to: B2.type = B2

    override def pointwise[T <: from.VBound & to.VBound]: from.Element[T] => to.Element[T] =
      v => v.asInstanceOf[to.Element[T]]
  }

  describe("Converter") {

    it("empty") {
      val v1 = B1.Eye
      val v2 = Conv(v1)
      assert(v2 == B2.Eye)
    }

    it("single") {
      //      import B1._
      //      import B1.tupleOps

      val v1 = 1 ><: B1.Eye
      val v2 = Conv(v1)
      assert(v2 == 1 ><: B2.Eye)
    }

    it("multiple") {

      val v1 = 1 ><: "a" ><: B1.Eye
      val v2 = Conv(v1)
      assert(v2 == 1 ><: "a" ><: B2.Eye)

    }
  }

  describe("Constrained") {

    object B3 extends InductiveBackbone {
      override type VBound = Int
      override type Element[V <: VBound] = V
    }

    object B4 extends InductiveBackbone {
      override type VBound = Int
      override type Element[V <: VBound] = V
    }

    object Conv2 extends Converter {
      override val from: B3.type = B3
      override val to: B4.type = B4

      override def pointwise[T <: from.VBound & to.VBound]: from.Element[T] => to.Element[T] =
        v => v.asInstanceOf[to.Element[T]]
    }

    it("pass") {
      val v1 = 1 ><: B3.Eye
      val v2 = Conv2(v1)
      assert(v2 == 1 ><: B4.Eye)
    }

  }
}
