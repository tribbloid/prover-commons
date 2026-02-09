package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.testlib.BaseSpec
import shapeless.HNil

class RecursiveHeapBackboneSpec extends BaseSpec {

  object Fixture extends RecursiveHeapBackbone {
    type VBound = Any
  }
  import Fixture.*

  describe("Empty") {
    it("asTuple should return HNil") {
      assert(Empty.tuple == HNil)
    }

    it("asList should return Nil") {
      assert(Empty.asList == List.empty)
    }

    it("toString should return correct string") {
      assert(Empty.toString == RecursiveHeapBackbone.EMPTY)
    }
    it("should behave as a Product") {
      assert(Empty.productArity == 0)
      assert(Empty.productIterator.isEmpty)
      assert(Empty.canEqual(Empty))
      assert(intercept[IndexOutOfBoundsException](Empty.productElement(0)).isInstanceOf[IndexOutOfBoundsException])
    }
  }

  describe("ConsImpl") {
    it("should work with simple values") {
      val tuple = 1 ><: "a" ><: Empty

      assert(tuple.head == 1)
      assert(tuple.tail.head == "a")
      assert(tuple.tail.tail == Empty)
    }

    it("asTuple should return correct HList") {
      val tuple = 1 ><: "a" ><: Empty
      val hlist = tuple.tuple

      assert(hlist == 1 :: "a" :: HNil)
    }

    it("asList should return correct List") {
      val tuple = 1 ><: "a" ><: Empty
      val list = tuple.asList

      assert(list == List(1, "a"))
    }

    it("deCons should deconstruct correctly") {
      val tuple = 1 ><: Empty
      val (head, tail) = deCons(tuple)

      assert(head == 1)
      assert(tail == Empty)
    }

    it("cons should construct correctly") {
      val tuple = cons(1, Empty)
      assert(tuple.head == 1)
      assert(tuple.tail == Empty)
    }

    it("toString should be formatted correctly") {
      val tuple = 1 ><: "a" ><: Empty
      // The exact format depends on the implementation, checking for containment of elements
      assert(tuple.toString.contains("1"))
      assert(tuple.toString.contains("a"))
      assert(tuple.toString.contains("><:"))
    }
    it("should behave as a Product") {
      val tuple = 1 ><: Empty
      assert(tuple.productArity == 2)
      assert(tuple.productElement(0) == 1)
      assert(tuple.productElement(1) == Empty)
      assert(tuple.productIterator.toList == List(1, Empty))
      assert(tuple.canEqual(1 ><: Empty))
      assert(!tuple.canEqual(Empty))
    }

    it("should work with nested tuple 1 ><: 2 ><: Empty") {
      val tuple = 1 ><: 2 ><: Empty

      assert(tuple.head == 1)
      assert(tuple.tail.head == 2)
      assert(tuple.tail.tail == Empty)

      assert(tuple.productArity == 2)

      assert(tuple.productElement(0) == 1)
      assert(tuple.productElement(1) == 2 ><: Empty)

      assert(tuple.asList == List(1, 2))
    }
  }

  describe("ToFlatTuple") {
    it("should convert Empty to Unit") {
      val res = ToFlatTuple(Empty)
      assert(res == ())
    }

    it("should convert H ><: Empty to Tuple1(H)") {
      val tuple = 1 ><: Empty
      val res = ToFlatTuple(tuple)
      assert(res == Tuple1(1))
    }

    it("should convert H1 ><: H2 ><: Empty to (H1, H2)") {
      val tuple = 1 ><: "a" ><: Empty
      val res = ToFlatTuple(tuple)
      assert(res == (1, "a"))
    }

    it("should convert H1 ><: H2 ><: H3 ><: Empty to (H1, H2, H3)") {
      val tuple = 1 ><: "a" ><: true ><: Empty
      val res = ToFlatTuple(tuple)
      assert(res == (1, "a", true))
    }

    it("should convert H1 ><: H2 ><: H3 ><: H4 ><: Empty to (H1, H2, H3, H4)") {
      val tuple = 1 ><: "a" ><: true ><: 3.14 ><: Empty
      val res = ToFlatTuple(tuple)
      assert(res == (1, "a", true, 3.14))
    }
  }

  describe("ToFlat") {
    it("should convert Empty to Unit") {
      val res = ToFlat(Empty)
      assert(res == ())
    }

    it("should convert H ><: Empty to H") {
      val tuple = 1 ><: Empty
      val res = ToFlat(tuple)
      assert(res == 1)
    }

    it("should convert H1 ><: H2 ><: Empty to (H1, H2)") {
      val tuple = 1 ><: "a" ><: Empty
      val res = ToFlat(tuple)
      assert(res == (1, "a"))
    }

    it("should convert H1 ><: H2 ><: H3 ><: Empty to (H1, H2, H3)") {
      val tuple = 1 ><: "a" ><: true ><: Empty
      val res = ToFlat(tuple)
      assert(res == (1, "a", true))
    }

    it("should convert H1 ><: H2 ><: H3 ><: H4 ><: Empty to (H1, H2, H3, H4)") {
      val tuple = 1 ><: "a" ><: true ><: 3.14 ><: Empty
      val res = ToFlat(tuple)
      assert(res == (1, "a", true, 3.14))
    }
  }
}
