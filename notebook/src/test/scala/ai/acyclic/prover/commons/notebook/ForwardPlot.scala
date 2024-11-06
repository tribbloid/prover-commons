package ai.acyclic.prover.commons.notebook

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Local

import scala.collection.mutable

trait ForwardPlot {

  case class Forward(text: String) {

    val arrowBuffer = mutable.Buffer.empty[(Arrow.Outbound.^, Forward)]

    def from(fromNode: Forward, msg: String): this.type = {

      fromNode.arrowBuffer += Arrow.Outbound.NoInfo(Option(msg).filter(_.nonEmpty)) -> this
      this
    }
  }

  object Forward extends Local.Semilattice.Upper.Inspection[Forward] {

    case class node(value: Forward) extends _Node {

      override protected def getInduction: Seq[(_Arrow, Forward.this.node)] = {

        value.arrowBuffer.toSeq.map { v =>
          v._1 -> node(v._2)
        }
      }

    }

  }

//  object G {
//
//    case class Ops(value: Forward) extends Local.Semilattice.Upper.NodeImpl[Forward] {
//
//      override protected def inductionC: Seq[(_Arrow, Ops)] = {
//
//        value.arrowBuffer.toSeq.map { v =>
//          v._1 -> Ops(v._2)
//        }
//      }
//
//      override def nodeTextC: String = value.text
//    }
//
//    def apply(rootValues: Seq[Forward]) = Local.AnyGraph.makeTightest(
//      rootValues.map { v =>
//        Ops(v)
//      }: _*
//    )
//  }
}
