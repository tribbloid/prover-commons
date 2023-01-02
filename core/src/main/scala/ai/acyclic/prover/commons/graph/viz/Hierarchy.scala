package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.{viz, Semilattice}
import ai.acyclic.prover.commons.{Padding, TextBlock}

object Hierarchy extends Visualisations {

  override val graphSys: Semilattice.Upper.type = Semilattice.Upper

  implicit lazy val default: Hierarchy.Indent2.type = Hierarchy.Indent2

  trait BuiltIn extends Hierarchy {}

  case object Indent2 extends BuiltIn

  case object Indent2Minimal extends BuiltIn {

    override lazy val FORK: Padding = Padding("", "")
    override lazy val LEAF: Padding = Padding("", "")
    override lazy val SUB: Padding = Padding(" ‣ ", " : ")

    override lazy val DOT = ""
  }
}

trait Hierarchy extends Hierarchy.Format {

  lazy val FORK: Padding = Padding("-+", " :")
  lazy val LEAF: Padding = Padding("--", "  ")

  lazy val SUB: Padding = Padding(" !", " :")
  lazy val SUB_LAST: Padding = SUB.copy(body = SUB.body.map(_ => ' '))

  lazy val DOT = " "

  def apply[N <: Semilattice.Upper.Node](node: N): Viz[N] = Viz(node)

  case class Viz[N <: Semilattice.Upper.Node](node: N) extends Hierarchy.TextViz[N] {
    val outer: viz.Hierarchy.Format = Hierarchy.this

    lazy val treeString: String = {

      val wText = TextBlock(nodeString).indent(DOT)

      if (node.isLeaf) {

        wText.padLeft(LEAF).build

      } else {

        val selfT = wText.padLeft(FORK)

        val children: Seq[Semilattice.Upper.Node] = (node: Semilattice.Upper.Node).children

        val childrenTProtos: Seq[TextBlock] = children.map { child: Semilattice.Upper.Node =>
          val childViz = this.copy(child)
          TextBlock(childViz.treeString)
        }

        val childrenTMid = childrenTProtos.dropRight(1).map { tt =>
          tt.padLeft(SUB)
        }

        val childrenTLast = childrenTProtos.lastOption.map { tt =>
          tt.padLeft(SUB_LAST)
        }.toSeq

        val result = (Seq(selfT) ++ childrenTMid ++ childrenTLast)
          .map { v =>
            v.build
          }
          .mkString("\n")

        result
      }
    }
  }

}
