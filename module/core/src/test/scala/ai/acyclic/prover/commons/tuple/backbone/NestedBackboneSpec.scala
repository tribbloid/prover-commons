package ai.acyclic.prover.commons.tuple.backbone

import ai.acyclic.prover.commons.testlib.BaseSpec
import shapeless.HNil

class NestedBackboneSpec extends BaseSpec {

  object Fixture extends NestedBackbone {
    type VBound = Any
  }
  import Fixture.*

  describe("Empty") {
    it("asTuple should return HNil") {
      assert(Eye.HList == HNil)
    }

    it("asList should return Nil") {
      assert(Eye.asList == List.empty)
    }

    it("toString should return correct string") {
      assert(Eye.toString == NestedBackbone.EMPTY)
    }
    it("should behave as a Product") {
      assert(Eye.productArity == 0)
      assert(Eye.productIterator.isEmpty)
      assert(Eye.canEqual(Eye))
      assert(intercept[IndexOutOfBoundsException](Eye.productElement(0)).isInstanceOf[IndexOutOfBoundsException])
    }
  }

  describe("ConsImpl") {
    it("should work with simple values") {
      val tuple = 1 ><: "a" ><: Eye

      assert(tuple.head == 1)
      assert(tuple.tail.head == "a")
      assert(tuple.tail.tail == Eye)
    }

    it("asTuple should return correct HList") {
      val tuple = 1 ><: "a" ><: Eye
      val hlist = tuple.HList

      assert(hlist == 1 :: "a" :: HNil)
    }

    it("asList should return correct List") {
      val tuple = 1 ><: "a" ><: Eye
      val list = tuple.asList

      assert(list == List(1, "a"))
    }

    it("deCons should deconstruct correctly") {
      val tuple = 1 ><: Eye
      val (head, tail) = deCons(tuple)

      assert(head == 1)
      assert(tail == Eye)
    }

    it("cons should construct correctly") {
      val tuple = cons(1, Eye)
      assert(tuple.head == 1)
      assert(tuple.tail == Eye)
    }

    it("toString should be formatted correctly") {
      val tuple = 1 ><: "a" ><: Eye
      // The exact format depends on the implementation, checking for containment of elements
      assert(tuple.toString.contains("1"))
      assert(tuple.toString.contains("a"))
      assert(tuple.toString.contains("><:"))
    }

    it("should work with nested tuple 1 ><: 2 ><: Empty") {
      val tuple = 1 ><: 2 ><: Eye

      assert(tuple.head == 1)
      assert(tuple.tail.head == 2)
      assert(tuple.tail.tail == Eye)

      assert(tuple.asList == List(1, 2))
    }
  }
}
