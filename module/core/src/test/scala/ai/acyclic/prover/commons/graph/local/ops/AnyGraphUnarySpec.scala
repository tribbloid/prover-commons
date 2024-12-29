package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.graph.GraphFixture.GV.inspect
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.testlib.BaseSpec

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class AnyGraphUnarySpec extends BaseSpec {

  import GraphFixture.*

  it("Upcast") {
    val result = AnyGraphUnary
      .^(cyclic.make)
      .NodeUpcast[Any]

    result.text_flow.toString.shouldBe(
      cyclic.make.text_flow.toString
    )
  }

  describe("Traverse") {

    it("DepthFirst") {

      val down = mutable.Buffer.empty[GV]
      val up = mutable.Buffer.empty[GV]

      AnyGraphUnary
        .^(cyclic.make, 5)
        .Traverse(
          n => down += n.value,
          n => up += n.value
        )
        .DepthFirst

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

    it("DepthFirst_Once") {

      val down = mutable.Buffer.empty[GV]
      val up = mutable.Buffer.empty[GV]

      AnyGraphUnary
        .^(cyclic.make, 5)
        .Traverse(
          n => down += n.value,
          n => up += n.value
        )
        .DepthFirst_Once

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
      val result = AnyGraphUnary
        .^(diamond.make: Local.AnyGraph[GV], 4)
        .TransformLinear(
          GVRewriter(v => inspect(v)),
          down = { v =>
            val result = v.value.copy(text = v.value.text + "+" + inc.getAndIncrement(), Nil)
            result.children ++= v.value.children
            inspect(result)
          },
          up = { v =>
            val result = v.value.copy(text = v.value.text + "-" + inc.getAndIncrement(), Nil)
            result.children ++= v.value.children
            inspect(result)
          }
        )
      result
    }

    it("DepthFirst") {

      val tt = proto.DepthFirst

      tt.text_flow.toString.shouldBe(
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

      val tt = proto.DepthFirst_Once

      tt.text_flow.toString.shouldBe(
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

      val tt = proto.DepthFirst_Cached

      tt.text_flow.toString.shouldBe(
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
