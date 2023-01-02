package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.GraphFixture

class HasseSpec extends GraphFixture {

  describe("string") {

    it("cyclic graph") {

      cyclic.showHasseDiagram.treeString shouldBe
        """
          |  ┌───────┐
          |  │  bbb  │
          |  └┬───┬──┘
          |   │   │ ^
          |   v   │ │
          | ┌───┐ │ │
          | │ccc│ │ │
          | └─┬─┘ │ │
          |   │ ┌─┼─┘
          |   │ │ └───┐
          |   │ │     │
          |   v │     v
          | ┌───┴─┐ ┌───┐
          | │ aaa │ │ddd│
          | └─────┘ └───┘
          |""".stripMargin
    }

    it("diamond graph") {

      diamond.showHasseDiagram.treeString shouldBe
        """
          |  ┌─────┐
          |  │ aaa │
          |  └┬──┬─┘
          |   │  │
          |   │  └──┐
          |   │     │
          |   v     v
          | ┌───┐ ┌───┐
          | │ccc│ │bbb│
          | └──┬┘ └─┬─┘
          |    │    │
          |    │ ┌──┘
          |    │ │
          |    v v
          |  ┌─────┐
          |  │ ddd │
          |  └──┬──┘
          |     │
          |     v
          |   ┌───┐
          |   │eee│
          |   └───┘
          |""".stripMargin
    }
  }
}
