package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.testlib.BaseSpec

object HomSpec {}

class HomSpec extends BaseSpec {

  import ai.acyclic.prover.commons.function.Fixtures._
  import ai.acyclic.prover.commons.function.Hom._

  lazy val fn: Int :=> Int = { Fixtures._fn0 }

  lazy val chainedSelf = fn.andThen(fn)

  lazy val chainedOthers = fn.andThen[String] { v: Int =>
    s"${v}b"
  }

  lazy val fnText: String = {

    val result = fn.nodeText

    result.shouldBe(
      "_fn0 <at FnFixture.scala:7>"
    )

    fn.treeText.shouldBe(
      s"- ${result}"
    )

    result
  }

  describe("Fn") {

    it("chaining") {

      val r1 = chainedSelf.apply(1)

      chainedSelf.treeText.shouldBe(
        s"""
            |+ andThen
            |!-- ${fnText}
            |""".stripMargin
      )

      assert(r1 == 3)

      val r2 = chainedOthers.apply(1)

      chainedOthers.treeText.shouldBe(
        s"""
            |+ andThen
            |!-- ${fnText}
            |!-- chainedOthers <at HomSpec.scala:16>
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
