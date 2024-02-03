package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.testlib.BaseSpec

object PreDefSpec {}

class PreDefSpec extends BaseSpec {

  import ai.acyclic.prover.commons.function.Fixtures._
  import ai.acyclic.prover.commons.function.PreDef._

  lazy val fn: Int :=> Int = {

    Fixtures._fn0.treeText.shouldBe(
      "- _fn0$ <at FnFixture.scala:8>"
    )

    _fn0
  }

  describe("Fn") {

    it("chaining") {

      val chainedSelf: Impl.Fn[Int, Int] = fn.andThen(fn)
      val r1 = chainedSelf.apply(1)

      chainedSelf.treeText.shouldBe(
        """
            |+ andThen
            |!-- _fn0$ <at FnFixture.scala:8>
            |""".stripMargin
      )

      assert(r1 == 3)

      val chainedOthers: Impl.Fn[Int, String] = fn.andThen { v: Int =>
        v + "b"
      }
      val r2 = chainedOthers.apply(1)

      chainedOthers.treeText.shouldBe(
        """
            |+ andThen
            |!-- _fn0$ <at FnFixture.scala:8>
            |!--  <at PreDefSpec.scala:37>
            |""".stripMargin
      )

      assert(r2 == "2b")

    }
  }

  describe("Morphism") {}

  describe("Poly") {

    describe("summoning cases") {

      it("with input & output types") {

        _poly.at[Int].to[Int].summon
      }

      it("with input type only") {
        val v = _poly.at[Int].summon
        val v2 = _poly.at[Int].summon

        implicitly[v.type <:< _poly.Case[Int :=> Int]]
        assert(v == v2)

        val r = v.apply(1)
        r: Int
      }
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
