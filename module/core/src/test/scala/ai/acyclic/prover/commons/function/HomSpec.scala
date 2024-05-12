package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.fixture.Fixtures
import ai.acyclic.prover.commons.testlib.BaseSpec

object HomSpec {}

class HomSpec extends BaseSpec {

  import Fixtures._
  import ai.acyclic.prover.commons.function.Hom._

  lazy val fn: Int :=> Int = {

    val result = Fixtures.fn0

    result.toString.shouldBe(
      "fn0 <at Fns.scala:8>"
    )

    fn0.explain.treeText.shouldBe(
      s"- ${result}"
    )

    result
  }

  describe("Fn") {

    describe("composing with") {

      describe("self") {

        chainSelf.zipWithIndex.foreach {

          case ((fn, s), i) =>
            it(i.toString) {

              fn.explain.treeText.shouldBe(
                s
              )

              val r1 = fn.apply(1)
              assert(r1 == 3)
            }
        }
      }

      describe("others") {

        it("0") {

          val r2 = chainOther.apply(1)

          chainOther.explain.treeText.shouldBe(
            s"""
               |+ AndThen
               |!-- ${fn0Text}
               |!-- chainOther <at ChainOther.scala:7>
               |""".stripMargin
          )

          assert(r2 == "2b")
        }
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
