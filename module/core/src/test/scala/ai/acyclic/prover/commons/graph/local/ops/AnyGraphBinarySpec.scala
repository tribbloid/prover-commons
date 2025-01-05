package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.testlib.BaseSpec

class AnyGraphBinarySpec extends BaseSpec {

  import GraphFixture.*

  describe("Union") {

    it("of semilattice") {

      val g1 = Local(diamond*)
      val g2 = Local(Appendage.p1*)

      val uu = g1.><(g2).Union()

      uu.text_flow()
        .toString
        .shouldBe(
          """
          |   ┌─────┐
          |   │ aaa │
          |   └┬──┬─┘
          |    │  │
          |    │  └───┐
          |    │      │
          |    v      v
          | ┌─────┐ ┌───┐
          | │ bbb │ │ccc│
          | └─┬─┬─┘ └─┬─┘
          |   │ │     │
          | ┌─┘ │ ┌───┘
          | │   │ │
          | │   v v
          | │ ┌─────┐
          | │ │ ddd │
          | │ └───┬─┘
          | └───┐ │
          |     │ │
          |     v v
          |   ┌─────┐
          |   │ eee │
          |   └─────┘
          |""".stripMargin
        )
    }

    it("of graph") {

      val g1 = Local(cyclic*)
      val g2 = Local(Appendage.p1*)

      val uu = g1.><(g2).Union()

      uu.text_flow()
        .toString
        .shouldBe(
          """
          |    ┌─────────┐
          |    │   bbb   │
          |    └─┬┬─┬────┘
          |      ││ │ ^
          |   ┌──┘│ │ │
          |   │   │ │ │
          |   v   │ │ │
          | ┌───┐ │ │ │
          | │ccc│ │ │ │
          | └─┬─┘ │ │ │
          |   │┌──┘ │ │
          |   ││    └─┼─────┐
          |   └┼─────┐│     │
          |    │     ││     │
          |    v     v│     v
          | ┌───┐ ┌───┴─┐ ┌───┐
          | │eee│ │ aaa │ │ddd│
          | └───┘ └─────┘ └───┘
          |""".stripMargin
        )
    }
  }
}
