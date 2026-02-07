package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.testlib.BaseSpec
import shapeless.{::, HNil}

class ToTupleBackboneSpec extends BaseSpec {

  object Fixture extends ToTupleBackbone {
    type VBound = Any
  }
  import Fixture.*

  describe("Empty") {
    it("asTuple should return HNil") {
      assert(Empty.tuple == HNil)
    }

    it("asList should return Nil") {
      assert(Empty.asList == Nil)
    }

    it("toString should return correct string") {
      assert(Empty.toString == ToTupleBackbone.EMPTY)
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
      val tuple = Empty >< 1 >< "a"

      assert(tuple.head == "a")
      assert(tuple.tail.head == 1)
      assert(tuple.tail.tail == Empty)
    }

    it("asTuple should return correct HList") {
      val tuple = Empty >< 1 >< "a"
      val hlist = tuple.tuple

      assert(hlist == "a" :: 1 :: HNil)
    }

    it("asList should return correct List") {
      val tuple = Empty >< 1 >< "a"
      val list = tuple.asList

      assert(list == List(1, "a"))
    }

    it("deCons should deconstruct correctly") {
      val tuple = Empty >< 1
      val (tail, head) = deCons(tuple)

      assert(tail == Empty)
      assert(head == 1)
    }

    it("cons should construct correctly") {
      val tuple = cons(Empty, 1)
      assert(tuple.head == 1)
      assert(tuple.tail == Empty)
    }

    it("toString should be formatted correctly") {
      val tuple = Empty >< 1 >< "a"
      // The exact format depends on the implementation, checking for containment of elements
      assert(tuple.toString.contains("1"))
      assert(tuple.toString.contains("a"))
      assert(tuple.toString.contains("><"))
    }
    it("should behave as a Product") {
      val tuple = Empty >< 1
      assert(tuple.productArity == 2)
      assert(tuple.productElement(0) == Empty)
      assert(tuple.productElement(1) == 1)
      assert(tuple.productIterator.toList == List(Empty, 1))
      assert(tuple.canEqual(Empty >< 1))
      assert(!tuple.canEqual(Empty))
    }

    it("should work with nested tuple Empty >< 1 >< 2") {
      val tuple = Empty >< 1 >< 2

      assert(tuple.head == 2)
      assert(tuple.tail.head == 1)
      assert(tuple.tail.tail == Empty)

      assert(tuple.productArity == 2) // TODO: this is not idiomatic, should be 3

      assert(tuple.productElement(0) == Empty >< 1)
      assert(tuple.productElement(1) == 2)

      assert(tuple.asList == List(1, 2))
    }
  }
}
