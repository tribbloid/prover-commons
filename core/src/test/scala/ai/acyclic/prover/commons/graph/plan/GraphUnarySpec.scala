package ai.acyclic.prover.commons.graph.plan

import ai.acyclic.prover.commons.graph.GraphFixture
import ai.acyclic.prover.commons.graph.plan.local.GraphUnary
import org.scalatest.funspec.AnyFunSpec

import scala.collection.mutable

class GraphUnarySpec extends AnyFunSpec with GraphFixture {

  import GraphFixture._

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

  describe("Transform") {

    it("down") {}

    it("up") {}
  }

}
