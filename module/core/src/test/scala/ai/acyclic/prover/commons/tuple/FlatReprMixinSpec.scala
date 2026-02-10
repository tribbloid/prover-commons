package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.tuple.backbone.RecursiveHeapBackbone
import shapeless.*
import shapeless.ops.hlist.Tupler
import ai.acyclic.prover.commons.tuple.Tuples

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

    it("debug Tupler on intersection types") {
      type L = (Int with Any) :: (Int with Any) :: HNil
      // This should compile if Tupler handles intersection types
      val tupler = implicitly[Tupler[L]]
      val l: L = 1 :: 2 :: HNil
      assert(tupler(l) == (1, 2))
    }

    it("debug ToTuple") {
      val t = TestBackbone.cons(1, TestBackbone.Empty)
      val h = TestBackbone.ToTuple(t)
      assert(h == 1 :: HNil)
    }

    it("debug FromTuple implicit resolution") {
      val tuple = (1, 2, 3)
      type T = (Int, Int, Int)
      val gen = implicitly[Generic[T]]
      println(s"Generic found: ${gen.to(tuple)}")
      val fromTuple = implicitly[TestBackbone.FromTuple.|-[gen.Repr, TestBackbone.Inductive]]
      println(s"FromTuple found: $fromTuple")
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

      val result = TestBackbone.FromFlatRepr("a")
      assert(result == TestBackbone.cons("a", TestBackbone.Empty))
    }

    it("should convert scala tuple 2 to 2-element tuple") {
      import TestBackbone.FromFlatRepr.*

      val result = TestBackbone.FromFlatRepr((1, 2))
      assert(result == TestBackbone.cons(1, TestBackbone.cons(2, TestBackbone.Empty)))
    }

    it("should convert scala tuple 3 to 3-element tuple") {
      import TestBackbone.FromFlatRepr.*

      val result = TestBackbone.FromFlatRepr((1, 2, 3))
      assert(result == TestBackbone.cons(1, TestBackbone.cons(2, TestBackbone.cons(3, TestBackbone.Empty))))
    }

    it("should convert scala tuple 4 to 4-element tuple") {
      import TestBackbone.FromFlatRepr.*

      val result = TestBackbone.FromFlatRepr((1, 2, 3, 4))
      val expected =
        TestBackbone.cons(1, TestBackbone.cons(2, TestBackbone.cons(3, TestBackbone.cons(4, TestBackbone.Empty))))
      assert(result == expected)
    }
  }

}

object FlatReprMixinSpec {

  // Custom backbone for testing - same as TupleConverterMixinSpec
  object TestBackbone extends RecursiveHeapBackbone {
    override type VBound = Any
  }

}
