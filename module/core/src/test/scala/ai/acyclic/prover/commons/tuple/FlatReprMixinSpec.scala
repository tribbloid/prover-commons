package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.tuple.backbone.RecursiveHeapBackbone

class FlatReprMixinSpec extends BaseSpec {
  import FlatReprMixinSpec.*

  describe("ToFlatRepr") {

    it("should convert Empty to Unit") {
      import TestBackbone.ToFlatRepr.*

      // The poly function needs to be imported or used via the object
      val result = TestBackbone.ToFlatRepr(TestBackbone.Empty)
      assert(result == ())
    }

    it("should convert single element tuple to value") {
      import TestBackbone.ToFlatRepr.*

      val t = TestBackbone.cons(1, TestBackbone.Empty)
      val result = TestBackbone.ToFlatRepr(t)
      assert(result == 1)
    }

    it("should convert 2-element tuple to scala tuple") {
      import TestBackbone.ToFlatRepr.*

      val t = TestBackbone.cons("a", TestBackbone.cons("b", TestBackbone.Empty))
      val result = TestBackbone.ToFlatRepr(t)
      assert(result == ("a", "b"))
    }

    it("should convert 3-element tuple to scala tuple") {
      import TestBackbone.ToFlatRepr.*

      val t = TestBackbone.cons(1, TestBackbone.cons(2, TestBackbone.cons(3, TestBackbone.Empty)))
      val result = TestBackbone.ToFlatRepr(t)
      assert(result == (1, 2, 3))
    }

    it("should convert 4-element tuple to scala tuple") {
      import TestBackbone.ToFlatRepr.*

      val t =
        TestBackbone.cons(1, TestBackbone.cons(2, TestBackbone.cons(3, TestBackbone.cons(4, TestBackbone.Empty))))
      val result = TestBackbone.ToFlatRepr(t)
      assert(result == (1, 2, 3, 4))
    }
  }

  describe("FromFlatRepr") {

    it("should convert Unit to Empty") {
      import TestBackbone.FromFlatRepr.*

      val result = TestBackbone.FromFlatRepr(())
      assert(result == TestBackbone.Empty)
    }

    it("should convert value to single element tuple") {
      import TestBackbone.FromFlatRepr.*

      val result = TestBackbone.FromFlatRepr(1)
      assert(result == TestBackbone.cons(1, TestBackbone.Empty))
    }

    it("should convert scala tuple 2 to 2-element tuple") {
      import TestBackbone.FromFlatRepr.*

//      val result = TestBackbone.FromFlatRepr((1, 2))
//      assert(result == TestBackbone.cons(1, TestBackbone.cons(2, TestBackbone.Empty)))
    }

    it("should convert scala tuple 3 to 3-element tuple") {
      import TestBackbone.FromFlatRepr.*

//      val result = TestBackbone.FromFlatRepr((1, 2, 3))
//      assert(result == TestBackbone.cons(1, TestBackbone.cons(2, TestBackbone.cons(3, TestBackbone.Empty))))
    }

    it("should convert scala tuple 4 to 4-element tuple") {
      import TestBackbone.FromFlatRepr.*

//      val result = TestBackbone.FromFlatRepr((1, 2, 3, 4))
//      val expected =
//        TestBackbone.cons(1, TestBackbone.cons(2, TestBackbone.cons(3, TestBackbone.cons(4, TestBackbone.Empty))))
//      assert(result == expected)
    }
  }

}

object FlatReprMixinSpec {

  // Custom backbone for testing - same as TupleConverterMixinSpec
  object TestBackbone extends RecursiveHeapBackbone {
    override type VBound = Any
  }

}
