package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.testlib.BaseSpec

class FlowSpec extends BaseSpec {

  import GraphFixture._

  describe("string") {

    it("cyclic graph") {

      cyclic.graph.diagram_flow.toString shouldBe
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

      cyclic.graphWithArrowText.diagram_flow.toString shouldBe
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

      diamond.graph.diagram_flow.toString shouldBe
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

      diamond.graphWithArrowText.diagram_flow.toString shouldBe
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
}
