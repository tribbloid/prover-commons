package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.graph.ops.AnyGraphUnary
import ai.acyclic.prover.commons.testlib.BaseSpec

class AnyGraphBinarySpec extends BaseSpec {

  import GraphFixture.*

  describe("Union") {

    it("of semilattice") {

      val g1: GraphFixture.GV.Graph_ = diamond.make
      val g2: GraphFixture.GV.Graph_ = Appendage.p1.make

      val uu = AnyGraphUnary.^(g1).><(g2).Union[GraphFixture.GV]()

      uu.text_flow.toString.shouldBe(
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

      val g1 = cyclic.make
      val g2 = Appendage.p1.make

      val uu = AnyGraphUnary.^(g1).><(g2).Union[GraphFixture.GV]()

      uu.text_flow.toString.shouldBe(
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
