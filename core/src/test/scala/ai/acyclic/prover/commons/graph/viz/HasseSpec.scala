package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.GraphFixture

class HasseSpec extends GraphFixture {

  describe("string") {

    it("cyclic graph") {

      cyclic.graph.diagram_Hasse.treeString shouldBe
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

      cyclic.graphWithArrowText.diagram_Hasse.treeString shouldBe
        """
          |         ┌──────────────┐
          |         │( aaa |> bbb )│
          |         ├──────────────┤
          |         │     bbb      │
          |         └─┬──────────┬─┘
          |           │         ^│
          |           │         │└────┐
          |           v         │     │
          |   ┌──────────────┐  │     │
          |   │( bbb |> ccc )│  │     │
          |   ├──────────────┤  │     │
          |   │     ccc      │  │     │
          |   └──┬───────────┘  │     │
          |      │     ┌────────┘     │
          |      v     │              v
          | ┌──────────┴───┐ ┌──────────────┐
          | │( ccc |> aaa )│ │( bbb |> ddd )│
          | ├──────────────┤ ├──────────────┤
          | │     aaa      │ │     ddd      │
          | └──────────────┘ └──────────────┘
          |""".stripMargin
    }

    it("diamond graph") {

      diamond.graph.diagram_Hasse.treeString shouldBe
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

      diamond.graphWithArrowText.diagram_Hasse.treeString shouldBe
        """
          |             ┌─────┐
          |             │ aaa │
          |             └─┬─┬─┘
          |               │ │
          |               │ └────────┐
          |               │          │
          |               v          v
          | ┌──────────────┐ ┌──────────────┐
          | │( aaa |> bbb )│ │( aaa |> ccc )│
          | ├──────────────┤ ├──────────────┤
          | │     bbb      │ │     ccc      │
          | │     [0]      │ │     [1]      │
          | └──────────┬───┘ └───┬──────────┘
          |            │         │
          |            v         v
          |  ┌────────────────────────────┐
          |  │⎛ from [0]   ⎫⎛ from [1]   ⎫│
          |  │⎝ bbb |> ddd ⎭⎝ ccc |> ddd ⎭│
          |  ├────────────────────────────┤
          |  │            ddd             │
          |  └──────────────┬─────────────┘
          |                 │
          |                 v
          |         ┌──────────────┐
          |         │( ddd |> eee )│
          |         ├──────────────┤
          |         │     eee      │
          |         └──────────────┘
          |""".stripMargin
    }
  }

  it("graph with duplicate nodes") {

    withDuplicateNodes.graph.diagram_Hasse.treeString shouldBe
      """
        |    ┌───┐
        |    │aaa│
        |    └─┬─┘
        |      │
        |      v
        |  ┌───────┐
        |  │  bbb  │
        |  └┬────┬─┘
        |   │ ^  │
        |   │ │  └──┐
        |   │ │     │
        |   v │     v
        | ┌───┴─┐ ┌───┐
        | │ ccc │ │ddd│
        | └─────┘ └───┘
        |""".stripMargin
  }
}
