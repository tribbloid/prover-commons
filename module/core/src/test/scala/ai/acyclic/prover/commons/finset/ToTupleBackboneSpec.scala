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
      assert(Empty.asTuple == HNil)
    }

    it("asList should return Nil") {
      assert(Empty.asList == Nil)
    }

    it("toString should return correct string") {
      assert(Empty.toString == ToTupleBackbone.EMPTY)
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
      val hlist = tuple.asTuple

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
  }
}
