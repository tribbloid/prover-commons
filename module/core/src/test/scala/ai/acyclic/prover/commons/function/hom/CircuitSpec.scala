package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.testlib.BaseSpec

object CircuitSpec {}

class CircuitSpec extends BaseSpec {

  import ai.acyclic.prover.commons.function.fixture.Circuits._

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

            val normal = fn.normalize
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

            val normal = fn.normalize
            normal.explain.text_hierarchy.shouldBe(
              s
            )

            val r1 = fn.apply(1)
            assert(r1 == "2b")
          }

      }
    }

    describe("twice") {

      import ai.acyclic.prover.commons.function.fixture.ChainTwice

      ChainTwice.pairs.zipWithIndex.foreach {

        case ((fn, s), i) =>
          it(i.toString) {

            val normal = fn.normalize
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
    import ai.acyclic.prover.commons.function.fixture.PointwiseAndChain

    PointwiseAndChain.pairs.zipWithIndex.foreach {

      case ((fn, s), i) =>
        it(i.toString) {

          val normal = fn.normalize
          normal.explain.text_hierarchy.shouldBe(
            s
          )

          val r1 = fn.apply(1 -> 2L)
          assert(r1 == List(1, 2, 3) -> List(2.0, 2.1, 2.2))
        }
    }
  }

  describe("higher-order") {

    import ai.acyclic.prover.commons.function.fixture.HigherOrder1

    HigherOrder1.pairs.zipWithIndex.foreach {

      case ((fn, s), i) =>
        it(i.toString) {

          val normal = fn.normalize
          normal.explain.text_hierarchy.shouldBe(
            s
          )

//          val r1 = fn.apply(1 -> 2L)
        }
    }
  }

}
