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

    it("should convert two element tuple to Scala Tuple2") {
      import TestBackbone.ToFlatRepr.*

      val t = TestBackbone.cons(1, TestBackbone.cons("a", TestBackbone.Empty))
      val result = TestBackbone.ToFlatRepr(t)
      assert(result == (1, "a"))
    }

    it("should convert three element tuple to Scala Tuple3") {
      import TestBackbone.ToFlatRepr.*

      val t = TestBackbone.cons(1, TestBackbone.cons("a", TestBackbone.cons(true, TestBackbone.Empty)))
      val result = TestBackbone.ToFlatRepr(t)
      assert(result == (1, "a", true))
    }
  }

}

object FlatReprMixinSpec {

  // Custom backbone for testing - same as TupleConverterMixinSpec
  object TestBackbone extends RecursiveHeapBackbone {
    override type VBound = Any
  }

}
