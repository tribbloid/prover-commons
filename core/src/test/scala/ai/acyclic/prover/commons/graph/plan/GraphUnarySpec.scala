package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.graph.plan.local.GraphUnary
import org.scalatest.funspec.AnyFunSpec

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

class GraphUnarySpec extends AnyFunSpec with GraphFixture {

  import GraphFixture._

  it("Upcast") {
    val result = GraphUnary(cyclic.graph)
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

      GraphUnary(cyclic.graph)
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

      GraphUnary(cyclic.graph)
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

    it("down") {

      val inc = new AtomicInteger(0)
      val transformed = GraphUnary(cyclic.graph)
        .TransformLinear(
          GNRewriter,
          5,
          down = { v =>
            val result = v.copy(text = v.text + inc.getAndIncrement())
            result.children ++= v.children
            result
          }
        )
        .DepthFirst
        .exe

      transformed.diagram_Hasse.treeString.shouldBe(
      )
    }

    it("up") {

      val inc = new AtomicInteger(0)
      val transformed = GraphUnary(cyclic.graph)
        .TransformLinear(
          GNRewriter,
          5,
          up = { v =>
            val result = v.copy(text = v.text + inc.getAndIncrement())
            result.children ++= v.children
            result
          }
        )
        .DepthFirst
        .exe

      transformed.diagram_Hasse.treeString.shouldBe(
      )
    }
  }

}
