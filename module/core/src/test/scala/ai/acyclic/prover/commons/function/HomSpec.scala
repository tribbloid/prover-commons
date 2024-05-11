package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.fixture.Fixtures
import ai.acyclic.prover.commons.testlib.BaseSpec

object HomSpec {}

class HomSpec extends BaseSpec {

  import Fixtures._
  import ai.acyclic.prover.commons.function.Hom._

  lazy val fn: Int :=> Int = { Fixtures.fn0 }

  lazy val fnText: String = {

    val result = fn.explain.nodeText

    result.shouldBe(
      "fn0 <at Fns.scala:8>"
    )

    fn.explain.treeText.shouldBe(
      s"- ${result}"
    )

    result
  }

  describe("Fn") {

    describe("chaining") {

      it("1") {

        val r1 = chainSelf.apply(1)

        chainSelf.explain.treeText.shouldBe(
          s"""
             |+ AndThen
             |!-- ${fnText}
             |!-- ${fnText}
             |""".stripMargin
        )

        assert(r1 == 3)
      }

      it("2") {

        val r2 = chainOther.apply(1)

        chainOther.explain.treeText.shouldBe(
          s"""
             |+ AndThen
             |!-- ${fnText}
             |!-- chainOther <at ChainOther.scala:7>
             |""".stripMargin
        )

        assert(r2 == "2b")
      }

    }
  }

  describe("Mono") {}

  describe("Poly") {

    describe("summoning cases") {

      it("with input & output types") {

        _poly.at[Int].to[Int].summon
      }

      it("with input type only") {
        val v = _poly.at[Int].summon
        val v2 = _poly.at[Int].summon

        implicitly[v.type <:< (_poly.=>>[Int, Int])]
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
