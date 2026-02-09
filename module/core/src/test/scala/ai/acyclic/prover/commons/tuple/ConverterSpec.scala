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

    it("should convert Empty") {
      val converted = Conv.emptyCase(B1.Empty)
      assert(converted == B2.Empty)
    }

    it("should convert single element tuple") {
      val t1 = B1.cons(1, B1.Empty)
      // We need explicit implicit resolution or explicit call
      // The implicit inside Conv should handle it if imported or called explicitly

      // Let's rely on explicit application first to test logic
      val converted = Conv.inductiveCase(Conv.emptyCase, implicitly[Int <:< Any]).apply(t1)

      assert(converted.head == 1)
      assert(converted.tail == B2.Empty)
    }

    it("should convert multi-element tuple") {
      val t1 = B1.cons(1, B1.cons("a", B1.Empty))

      // Construct the converter recursively
      val conv = Conv.inductiveCase(
        Conv.inductiveCase(Conv.emptyCase, implicitly[String <:< Any]),
        implicitly[Int <:< Any]
      )

      val converted = conv.apply(t1)

      assert(converted.head == 1)
      assert(converted.tail.head == "a")
      assert(converted.tail.tail == B2.Empty)
    }

    it("should implicit resolution work") {
      // Import implicits to Scope
      import Conv._

      val t1 = B1.cons(1, B1.cons("a", B1.Empty))

      // We need to summon the implicit Converter
      // The type of t1 is B1.><:[Int, B1.><:[String, B1.Empty]] (roughly)
      // We need a Hom from this to B2 equivalent.

      // Converter extends Hom.Poly.
      val converted = Conv(t1).asInstanceOf[B2.Inductive]

      // Verify values
      // Since we don't strictly know the output type in this loose test without precise type tracking
      // We check runtime values

      assert(converted.asList == List(1, "a"))
    }
  }
}
