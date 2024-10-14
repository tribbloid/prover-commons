package ai.acyclic.prover.commons.function.hom

import ai.acyclic.prover.commons.function.fixture.{ChainOther, ChainSelf}
import ai.acyclic.prover.commons.testlib.BaseSpec

object CircuitSpec {}

class CircuitSpec extends BaseSpec {

  import ai.acyclic.prover.commons.function.fixture.Circuits._

  it("explain") {

    fn0.explain.nodeText.shouldBe(
      "fn0 <at Fns.scala:7>"
    )

    fn0.toString.shouldBe(
      fn0.explain.nodeText
    )

    fn0.explain.treeText.shouldBe(
      s"- ${fn0.toString}"
    )
  }

  describe("chain") {

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

    it("ref") {

      val d = ChainOther.s2

      val text = d.explain.treeText

    }

    describe("other") {

      import ai.acyclic.prover.commons.function.fixture.ChainOther._

      it("0") {

        val r2 = s1.apply(1)

        s1.explain.treeText.shouldBe(
          s"""
               |+ Compose
               |!-- ${fn0.explain.nodeText}
               |!-- s1 <at ChainOther.scala:9>
               |""".stripMargin
        )

        assert(r2 == "2b")
      }
    }

//      describe("HOF") {}
  }

}
