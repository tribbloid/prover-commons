package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.graph.plan.local.GraphUnary
import org.scalatest.funspec.AnyFunSpec

import scala.collection.mutable

class GraphPlansSpec extends AnyFunSpec with GraphFixture {

  import GraphFixture._

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
        |ccc
        |ddd
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
        |ccc
        |ddd
        |bbb
        |aaa
        |""".stripMargin
      )
  }

  it(" ... _ForEach") {

    val down = mutable.Buffer.empty[GN]
    val up = mutable.Buffer.empty[GN]

    GraphUnary(cyclic.graph)
      .Traverse(
        5,
        v => down += v,
        v => up += v
      )
      .DepthFirst_ForEach
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
