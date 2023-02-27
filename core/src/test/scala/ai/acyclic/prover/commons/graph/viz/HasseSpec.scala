package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.GraphFixture

class HasseSpec extends GraphFixture {

  describe("string") {

    it("cyclic graph") {

      cyclic.graph.showHasseDiagram.treeString shouldBe
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

      cyclic.graphWithArrowText.showHasseDiagram.treeString shouldBe
        """
          |         ┌──────────────┐
          |         │[ aaa |> bbb ]│
          |         │     bbb      │
          |         └─┬──────────┬─┘
          |           │         ^│
          |           │         │└────┐
          |           v         │     │
          |   ┌──────────────┐  │     │
          |   │[ bbb |> ccc ]│  │     │
          |   │     ccc      │  │     │
          |   └──┬───────────┘  │     │
          |      │     ┌────────┘     │
          |      v     │              v
          | ┌──────────┴───┐ ┌──────────────┐
          | │[ ccc |> aaa ]│ │[ bbb |> ddd ]│
          | │     aaa      │ │     ddd      │
          | └──────────────┘ └──────────────┘
          |""".stripMargin
    }

    it("diamond graph") {

      diamond.graph.showHasseDiagram.treeString shouldBe
        """
          |  ┌─────┐
          |  │ aaa │
          |  └┬──┬─┘
          |   │  │
          |   │  └──┐
          |   │     │
          |   v     v
          | ┌───┐ ┌───┐
          | │bbb│ │ccc│
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

      diamond.graphWithArrowText.showHasseDiagram.treeString shouldBe
        """
          |              ┌─────┐
          |              │ aaa │
          |              └─┬─┬─┘
          |                │ │
          |         ┌──────┘ └───────┐
          |         │                │
          |         v                v
          | ┌──────────────┐ ┌──────────────┐
          | │[ aaa |> bbb ]│ │[ aaa |> ccc ]│
          | │   bbb [0]    │ │   ccc [1]    │
          | └──────────┬───┘ └────┬─────────┘
          |            │          │
          |            v          v
          | ┌────────────────────────────────┐
          | │[ 0 bbb |> ddd ][ 1 ccc |> ddd ]│
          | │              ddd               │
          | └───────────────┬────────────────┘
          |                 │
          |                 v
          |         ┌──────────────┐
          |         │[ ddd |> eee ]│
          |         │     eee      │
          |         └──────────────┘
          |""".stripMargin
    }
  }
}
