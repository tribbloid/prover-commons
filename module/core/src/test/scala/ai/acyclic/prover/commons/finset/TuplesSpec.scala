package ai.acyclic.prover.commons.finset

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.finset.Tuples.*
import shapeless.{::, HNil}

class TuplesSpec extends BaseSpec {

  describe("Tuples") {

    it("can construct and deconstruct") {

      val t: _0 >< Int >< String = _0 >< 1 >< "a"
      val t2 = Tuples.cons(Tuples.cons(_0, 1), "a")

      assert(t == t2)

      val (tail, head) = Tuples.deCons(t)
      assert(head == "a")
      assert(tail == _0 >< 1)
    }

    it("GetV") {
      val t: String :: Int :: HNil = _0 >< 1 >< "a"
      val inter = Tuples.InterOps(t)
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
      val t = _0 >< f

      val inter = Tuples.InterOps(t)
      val getField = inter.GetField
      import getField.getter

      val res = getField(k.narrow)
      assert(res == v)
    }
  }
}
