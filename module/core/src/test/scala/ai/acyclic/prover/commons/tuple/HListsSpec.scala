package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.tuple.HLists.*
import shapeless.{::, HNil}

class HListsSpec extends BaseSpec {

  describe("Tuples") {

    it("can construct and deconstruct") {

      val t: Int ><: String ><: Nil = 1 ><: "a" ><: _1
      val t2 = HLists.cons(1, HLists.cons("a", _1))

      assert(t == t2)

      val (head, tail) = HLists.deCons(t)
      assert(head == 1)
      assert(tail == "a" ><: _1)
    }

    it("GetV") {
      val t: Int :: String :: HNil = 1 ><: "a" ><: _1
      val inter = HLists.Ops(t)
      val getV = inter.GetV
      import getV.getter
      val v = getV(0)
      assert(v == 1)
    }

    it("GetField") {
      import shapeless.syntax.singleton.*
      val k: "k" = "k"
      val v = 1
      val f = shapeless.labelled.field[k.type](v)
      val t = f ><: _1

      val inter = HLists.Ops(t)
      val getField = inter.GetField
      import getField.getter

      val res = getField(k.narrow)
      assert(res == v)
    }
  }
}
