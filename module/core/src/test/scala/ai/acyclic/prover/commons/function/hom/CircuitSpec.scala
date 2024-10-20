package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.function.fixture._
import ai.acyclic.prover.commons.testlib.BaseSpec

object CircuitSpec {}

class CircuitSpec extends BaseSpec {

  import Circuits._

  describe("definition") {
    ignore("single-abstract method") { // disabled, current compiler is janky

//      val sam: Circuit.Impl[Int, Int] = { v =>
//        v
//      }
    }
  }

  it("explain") {

    fn0.explain.nodeText.shouldBe(
      "Blackbox(fn0 <at Circuits.scala:7>)"
    )

    fn0.toString.shouldBe(
      fn0.explain.nodeText
    )

    fn0.explain.text_hierarchy.shouldBe(
      s"- ${fn0.toString}"
    )
  }

  describe("chain") {

    import ai.acyclic.prover.commons.function.fixture.{ChainOther, ChainSelf}

    describe("self") {

      ChainSelf.pairs.zipWithIndex.foreach {

        case ((fn, s), i) =>
          it(i.toString) {

            val normal = fn.normalise
            normal.explain.text_hierarchy.shouldBe(
              s
            )

            val r1 = fn.apply(1)
            assert(r1 == 3)
          }
      }

    }

    describe("other") {

      ChainOther.pairs.zipWithIndex.foreach {

        case ((fn, s), i) =>
          it(i.toString) {

            val normal = fn.normalise
            normal.explain.text_hierarchy.shouldBe(
              s
            )

            val r1 = fn.apply(1)
            assert(r1 == "2b")
          }

      }
    }

    describe("twice") {

      ChainTwice.pairs.zipWithIndex.foreach {

        case ((fn, s), i) =>
          it(i.toString) {

            val normal = fn.normalise
            normal.explain.text_hierarchy.shouldBe(
              s
            )

            val r1 = fn.apply(1)
            assert(r1 == "10b")
          }
      }
    }

  }

  describe("pointwise") {

    PointwiseAndChain.pairs.zipWithIndex.foreach {

      case ((fn, s), i) =>
        it(i.toString) {

          val normal = fn.normalise
          normal.explain.text_hierarchy.shouldBe(
            s
          )

          val r1 = fn.apply(1 -> 2L)
          assert(r1 == List(3.0, 4.1, 5.2))
        }
    }
  }

  describe("higher-order") {

    import ai.acyclic.prover.commons.function.fixture.HigherOrder1

    HigherOrder1.pairs.zipWithIndex.foreach {

      case ((fn, s), i) =>
        it("1-" + i.toString) {

          val normal = fn.normalise
          normal.explain.text_hierarchy.shouldBe(
            s
          )
        }
    }

    HigherOrder2.pairs.zipWithIndex.foreach {

      case ((fn, s), i) =>
        it("2-" + i.toString) {

          val normal = fn.normalise
          normal.explain.text_hierarchy.shouldBe(
            s
          )

          //          val r1 = fn.apply(1 -> 2L)
        }
    }
  }

}
