package ai.acyclic.prover.commons.notebook

import ai.acyclic.prover.commons.debug.print_@
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.Topology.GraphT.OutboundT
import ai.acyclic.prover.commons.graph.local.Graph
import ai.acyclic.prover.commons.testlib.BaseSpec

import scala.collection.mutable

object PathToDOT {

  case class Forward(text: String) {

    val arrowBuffer = mutable.Buffer.empty[(Arrow.`~>`.^, Forward)]

    def from(fromNode: Forward, msg: String): this.type = {

      fromNode.arrowBuffer += Arrow.`~>`.NoInfo(Option(msg).filter(_.nonEmpty)) -> this
      this
    }
  }

  case class G(roots: Seq[Forward]) extends Graph.Outbound[Forward] {

    case class Ops(value: Forward) extends OutboundT.Ops[Forward] {

      override protected def getNodeText: String = value.text

      override protected def getInduction = {
        value.arrowBuffer.toSeq.map { v =>
          v._1 -> Ops(v._2)
        }
      }
    }
  }

  trait LambdaCube {

    val f = Forward("system F")

    val fOmega = Forward("system Fω").from(f, "+ higher kind")

    val lambdaP2 = Forward("system λP2").from(f, "+ dependent type")

    val coc = Forward("calculus of construction")
      .from(fOmega, "+ dependent type")
      .from(lambdaP2, "+ higher kind")
  }
  object LambdaCube extends LambdaCube

  trait ToDOT extends LambdaCube {

    val fSub = Forward("system F_<:")
      .from(
        f,
        """
        |+ subtype
        |+ union type
        |""".stripMargin.trim
      )

    val d = Forward("system D")
      .from(f, "+ path-dependent type")

    lambdaP2.from(d, "+ expression-dependent type")

    val dSub = Forward("system D_<:")
      .from(
        fSub,
        """
          |+ path-dependent type
          |+ path-dependent upper bound
          |""".stripMargin.trim
      )
      .from(
        d,
        """
          |+ subtype
          |+ union type
          |+ path-dependent upper bound
          |""".stripMargin.trim
      )

    val dSubBot = Forward("system D_<:>")
      .from(
        dSub,
        """
          |+ path-dependent lower bound
          |+ intersection type
          |""".stripMargin.trim
      )

    val dot = Forward("DOT")
      .from(
        dSubBot,
        """
          |+ object
          |+ type/value/method member
          |+ recursive self type
          |""".stripMargin.trim
      )

    val scala = Forward("Scala 3")
      .from(
        dot,
        "+ higher kind"
      )
  }
  object ToDOT extends ToDOT
}

class PathToDOT extends BaseSpec {

  import PathToDOT._

  it("lambda cube") {

    val g = G(Seq(LambdaCube.f))

    print_@(g.diagram_Hasse.treeString)

    print_@(g.diagram_linkedHierarchy.treeString)
  }

  it("DOT") {

    val g = G(Seq(ToDOT.f))

    print_@(g.diagram_Hasse.treeString)

    print_@(g.diagram_linkedHierarchy.treeString)
  }
}
