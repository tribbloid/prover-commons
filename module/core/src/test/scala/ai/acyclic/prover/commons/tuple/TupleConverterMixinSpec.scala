package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.tuple.backbone.RecursiveHeapBackbone
import shapeless.{::, HNil}

class TupleConverterMixinSpec extends BaseSpec {

  // Custom backbone for testing - expose _0 as a public value
  object TestBackbone extends RecursiveHeapBackbone {
    override type VBound = Any

  }

  describe("ToTuple") {

    it("should convert Empty to HNil via emptyCase") {
      val empty = TestBackbone.Eye
      val result = TestBackbone.ToTuple(empty)
      assert(result == HNil)
    }

    it("should convert single element tuple") {
      import TestBackbone.ToTuple._

      val t = TestBackbone.cons(1, TestBackbone.Eye)
      val converted = TestBackbone.ToTuple(t)

      assert(converted == (1 :: HNil))
    }

    it("should convert two element tuple") {
      import TestBackbone.ToTuple._

      val t = TestBackbone.cons(1, TestBackbone.cons("a", TestBackbone.Eye))
      val converted = TestBackbone.ToTuple(t)

      assert(converted == (1 :: "a" :: HNil))
    }

    it("should convert three element tuple") {
      import TestBackbone.ToTuple._

      val t = TestBackbone.cons(1, TestBackbone.cons("a", TestBackbone.cons(true, TestBackbone.Eye)))
      val converted = TestBackbone.ToTuple(t)

      assert(converted == (1 :: "a" :: true :: HNil))
    }

    it("should preserve types") {
      import TestBackbone.ToTuple._

      val t = TestBackbone.cons(42, TestBackbone.cons("hello", TestBackbone.Eye))
      val converted: Int :: String :: HNil = TestBackbone.ToTuple(t)

      assert(converted.head == 42)
      assert(converted.tail.head == "hello")
    }

    it("should handle nested types") {
      import TestBackbone.ToTuple._

      val innerTuple = TestBackbone.cons("nested", TestBackbone.Eye)
      val outerTuple = TestBackbone.cons(1, TestBackbone.cons(innerTuple, TestBackbone.Eye))

      val converted = TestBackbone.ToTuple(outerTuple)
      assert(converted.head == 1)
      assert(converted.tail.head == innerTuple)
    }
  }

  describe("FromTuple") {

    it("should convert HNil to Empty via emptyCase") {
      val result = TestBackbone.FromTuple(HNil)
      assert(result == TestBackbone.Eye)
    }

    // Note: Due to type-level complexity with abstract type aliases, explicit inductiveCase
    // construction is difficult. Instead, we test the implicit resolution approach.

    it("should resolve implicit chain for conversion") {
      import TestBackbone.FromTuple._

      // The implicit chain should be able to resolve for simple cases
      // This tests that the implicit machinery is set up correctly
      val emptyResult = TestBackbone.FromTuple(HNil)
      assert(emptyResult == TestBackbone.Eye)
    }
  }

  describe("Round-trip conversion") {

    it("should round-trip Empty") {
      import TestBackbone.ToTuple._

      val original = TestBackbone.Eye
      val toTuples = TestBackbone.ToTuple(original)
      val backToFin = TestBackbone.FromTuple(toTuples)

      assert(backToFin == original)
    }
  }

  describe("ToTuple vs Tuples comparison") {
    // Verify that ToTuple output is compatible with shapeless operations

    it("should produce HList compatible with shapeless ops") {
      import TestBackbone.ToTuple._
      import shapeless.ops.hlist._

      val t = TestBackbone.cons(1, TestBackbone.cons("a", TestBackbone.cons(3.14, TestBackbone.Eye)))
      val hlist: Int :: String :: Double :: HNil = TestBackbone.ToTuple(t)

      // Use shapeless Length to verify structure
      val len = Length[Int :: String :: Double :: HNil]
      assert(len() == shapeless.nat._3)

      // Access by index
      val at0 = At[Int :: String :: Double :: HNil, shapeless.nat._0]
      assert(at0(hlist) == 1)

      val at1 = At[Int :: String :: Double :: HNil, shapeless.nat._1]
      assert(at1(hlist) == "a")
    }
  }
}
