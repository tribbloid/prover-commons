package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.testlib.BaseSpec

object PreDefSpec {}

class PreDefSpec extends BaseSpec {

  import ai.acyclic.prover.commons.function.Fixtures._
  import ai.acyclic.prover.commons.function.PreDef._

  lazy val fn: Int :=> Int = {

    Fixtures._fn0.toString.shouldBe(
      "<defined at: FnFixture.scala:8>"
    )
    _fn0
  }

  describe("chaining") {

    it("function") {

      val chainedSelf = fn.andThen(fn)
      val r1 = chainedSelf(1)

      assert(r1 == 3)

      val chainedOthers = fn.andThen { v: Int =>
        v + "b"
      }
      val r2 = chainedOthers.apply(1)

      assert(r2 == "2b")
    }
  }

  describe("summoning Poly cases") {

    it("with input & output types") {

      val v = _poly.forCase[Int :=> Int].summon
      val v2 = _poly.forCase[PreDef.FnCompat[String, String]].summon
    }

    it("with input type only") {
      val v = _poly.at[Int].summon
      val v2 = _poly.under[Int].summon

      implicitly[v.type <:< _poly.Case[Int :=> Int]]
      assert(v == v2)

      val r = v(1)
      val t: Int = r
    }
  }
//  describe("Single Abstract Method definition for") {
//
//    it("function") {
//
//      val fn1: Tier1.Function[Int :: HNil, String] = { v =>
//        val _ = v._1 // uses implicits
//
//        val tt: Tuple1[Int] = v.asTuple // use dynamic selector
//
//        "" + tt._1
//      }
//    }
//  }
}
