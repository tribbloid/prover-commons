package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.fixture.{ChainSelf, Fns, HigherOrder1}
import ai.acyclic.prover.commons.testlib.BaseSpec

object HomSpec {}

class HomSpec extends BaseSpec {

  describe("Fn") {

    import Fns._

    it("explain") {

      fn0.explain.nodeText.shouldBe(
        "fn0 <at Fns.scala:8>"
      )

      fn0.toString.shouldBe(
        "fn0 <at Fns.scala:8>"
      )

      fn0.explain.treeText.shouldBe(
        s"- ${fn0.toString}"
      )
    }

    describe("composing with") {

      describe("self") {

        ChainSelf.pairs.zipWithIndex.foreach {

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

        import ai.acyclic.prover.commons.function.fixture.ChainOther._

        it("0") {

          val r2 = s1.apply(1)

          s1.explain.treeText.shouldBe(
            s"""
               |+ AndThen
               |!-- ${Fns.fn0.explain.nodeText}
               |!-- chainOther <at ChainOther.scala:7>
               |""".stripMargin
          )

          assert(r2 == "2b")
        }

        it("1") {

          val vv = HigherOrder1.flatMapOthers

          vv
        }
      }
    }
  }

  describe("Mono") {}

  describe("Poly") {

    import ai.acyclic.prover.commons.function.fixture.Polys._

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
