package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.graph.plan.local.GraphUnary
import org.scalatest.funspec.AnyFunSpec

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class GraphUnarySpec extends AnyFunSpec with GraphFixture {

  import GraphFixture._

  it("Upcast") {
    val result = GraphUnary
      .make(cyclic.graph)
      .UpcastNode[Any]()
      .exe

    result.diagram_Hasse.treeString.shouldBe(
      cyclic.graph.diagram_Hasse.treeString
    )
  }

  describe("Traverse") {

    it("DepthFirst") {

      val down = mutable.Buffer.empty[GN]
      val up = mutable.Buffer.empty[GN]

      GraphUnary
        .make(cyclic.graph)
        .Traverse(
          5,
          v => down += v,
          v => up += v
        )
        .DepthFirst
        .exe

      down
        .map(_.text)
        .mkString("\n")
        .shouldBe(
          """
            |aaa
            |bbb
            |ccc
            |aaa
            |bbb
            |ddd
            |""".stripMargin
        )
      up.map(_.text)
        .mkString("\n")
        .shouldBe(
          """
            |bbb
            |aaa
            |ccc
            |ddd
            |bbb
            |aaa
            |""".stripMargin
        )
    }

    it(" ... Once") {

      val down = mutable.Buffer.empty[GN]
      val up = mutable.Buffer.empty[GN]

      GraphUnary
        .make(cyclic.graph)
        .Traverse(
          5,
          v => down += v,
          v => up += v
        )
        .DepthFirst_Once
        .exe

      down
        .map(_.text)
        .mkString("\n")
        .shouldBe(
          """
            |aaa
            |bbb
            |ccc
            |ddd
            |""".stripMargin
        )
      up.map(_.text)
        .mkString("\n")
        .shouldBe(
          """
            |ccc
            |ddd
            |bbb
            |aaa
            |""".stripMargin
        )
    }
  }

  describe("Transform") {

    def proto = {
      val inc = new AtomicInteger(0)
      val result = GraphUnary
        .make(diamond.graph)
        .TransformLinear(
          GNRewriter,
          4,
          down = { v =>
            val result = v.copy(text = v.text + "+" + inc.getAndIncrement())
            result.children ++= v.children
            result
          },
          up = { v =>
            val result = v.copy(text = v.text + "-" + inc.getAndIncrement())
            result.children ++= v.children
            result
          }
        )
      result
    }

    it("DepthFirst") {

      val tt = proto.DepthFirst.exe

      tt.diagram_Hasse.treeString.shouldBe(
        """
          |     ┌────────┐
          |     │aaa+0-13│
          |     └──┬──┬──┘
          |        │  │
          |        │  └────┐
          |        │       │
          |        v       v
          | ┌───────┐ ┌────────┐
          | │bbb+1-6│ │ccc+7-12│
          | └───┬───┘ └────┬───┘
          |     │          │
          |     v          v
          | ┌───────┐ ┌────────┐
          | │ddd+2-5│ │ddd+8-11│
          | └───┬───┘ └────┬───┘
          |     │          │
          |     v          v
          | ┌───────┐ ┌────────┐
          | │eee+3-4│ │eee+9-10│
          | └───────┘ └────────┘
          |""".stripMargin
      )
    }

    it("DepthFirst_Once") {

      val tt = proto.DepthFirst_Once.exe

      tt.diagram_Hasse.treeString.shouldBe(
        """
          |     ┌───────┐
          |     │aaa+0-9│
          |     └──┬──┬─┘
          |        │  │
          |        │  └───┐
          |        │      │
          |        v      v
          | ┌───────┐ ┌───────┐
          | │bbb+1-6│ │ccc+7-8│
          | └───┬───┘ └─┬─────┘
          |     │       │
          |     v       v
          | ┌───────┐ ┌───┐
          | │ddd+2-5│ │ddd│
          | └───┬───┘ └─┬─┘
          |     │       │
          |     v       v
          | ┌───────┐ ┌───┐
          | │eee+3-4│ │eee│
          | └───────┘ └───┘
          |""".stripMargin
      )
    }

    it("DepthFirst_Cached") {

      val tt = proto.DepthFirst_Cached.exe

      tt.diagram_Hasse.treeString.shouldBe(
        """
          |     ┌───────┐
          |     │aaa+0-9│
          |     └──┬──┬─┘
          |        │  │
          |        │  └───┐
          |        │      │
          |        v      v
          | ┌───────┐ ┌───────┐
          | │bbb+1-6│ │ccc+7-8│
          | └──────┬┘ └───┬───┘
          |        │      │
          |        │  ┌───┘
          |        │  │
          |        v  v
          |     ┌───────┐
          |     │ddd+2-5│
          |     └───┬───┘
          |         │
          |         v
          |     ┌───────┐
          |     │eee+3-4│
          |     └───────┘
          |""".stripMargin
      )
    }
  }

}
