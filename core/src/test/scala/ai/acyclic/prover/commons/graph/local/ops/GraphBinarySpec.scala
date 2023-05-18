package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.testlib.BaseSpec

class GraphBinarySpec extends BaseSpec {

  import GraphFixture._

  describe("Union") {
    it("of semilattice") {

      val g1 = diamond.graph
      val g2 = Appendage.p1.graph

      val uu = GraphUnary.^(g1).&&(g2).Union().resolve

      uu.diagram_Hasse.graphString.shouldBe(
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

      val g1 = cyclic.graph
      val g2 = Appendage.p1.graph

      val uu = GraphUnary.^(g1).&&(g2).Union().resolve

      uu.diagram_Hasse.graphString.shouldBe(
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
