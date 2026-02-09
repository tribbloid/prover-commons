package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.tuple.backbone.RecursiveHeapBackbone
import shapeless.{::, HNil}

class FlatSchemaSpec extends BaseSpec {

  // Custom backbone for testing - same as TupleConverterMixinSpec
  object TestBackbone extends RecursiveHeapBackbone {
    override type VBound = Any

    // Expose protected _0 with a public alias
    val Empty_ : Empty = _0
  }

  describe("ToFlatRepr") {

    it("should convert Empty to Unit") {
      import TestBackbone.ToFlatRepr._

      val empty = TestBackbone.Empty_
      // The poly function needs to be imported or used via the object
      val result = TestBackbone.ToFlatRepr(empty)
      assert(result == ())
    }

    it("should convert single element tuple to value") {
      import TestBackbone.ToFlatRepr._

      val t = TestBackbone.cons(1, TestBackbone.Empty_)
      val result = TestBackbone.ToFlatRepr(t)
      assert(result == 1)
    }

    it("should convert two element tuple to Scala Tuple2") {
      import TestBackbone.ToFlatRepr._

      val t = TestBackbone.cons(1, TestBackbone.cons("a", TestBackbone.Empty_))
      val result = TestBackbone.ToFlatRepr(t)
      assert(result == (1, "a"))
    }

    it("should convert three element tuple to Scala Tuple3") {
      import TestBackbone.ToFlatRepr._

      val t = TestBackbone.cons(1, TestBackbone.cons("a", TestBackbone.cons(true, TestBackbone.Empty_)))
      val result = TestBackbone.ToFlatRepr(t)
      assert(result == (1, "a", true))
    }
  }

  describe("FromFlatRepr") {

    it("should convert Unit to Empty") {
      import TestBackbone.FromFlatRepr._

      val result = TestBackbone.FromFlatRepr(())
      assert(result == TestBackbone.Empty_)
    }

    it("should convert value to single element tuple") {
      import TestBackbone.FromFlatRepr._

      val result = TestBackbone.FromFlatRepr(1)
      val expected = TestBackbone.cons(1, TestBackbone.Empty_)
      assert(result == expected)
    }

    it("should convert Scala Tuple2 to two element tuple") {
      import TestBackbone.FromFlatRepr._

      val result = TestBackbone.FromFlatRepr((1, "a"))
      val expected = TestBackbone.cons(1, TestBackbone.cons("a", TestBackbone.Empty_))
      assert(result == expected)
    }

    it("should convert Scala Tuple3 to three element tuple") {
      import TestBackbone.FromFlatRepr._

      val result = TestBackbone.FromFlatRepr((1, "a", true))
      val expected = TestBackbone.cons(1, TestBackbone.cons("a", TestBackbone.cons(true, TestBackbone.Empty_)))
      assert(result == expected)
    }
  }

  describe("Round-trip conversion") {

    it("should round-trip single value") {
      val original = 42
      val intermediate = TestBackbone.FromFlatRepr(original)
      val back = TestBackbone.ToFlatRepr(intermediate)
      assert(back == original)
    }

    it("should round-trip tuple") {
      val original = (1, "a")
      val intermediate = TestBackbone.FromFlatRepr(original)
      val back = TestBackbone.ToFlatRepr(intermediate)
      assert(back == original)
    }
  }
}
