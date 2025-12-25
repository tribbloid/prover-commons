package ai.acyclic.prover.commons.notebook

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Local

import scala.collection.mutable

trait ForwardPlot {

  case class Forward(text: String) {

    val arrowBuffer = mutable.Buffer.empty[(Arrow.OutboundT.^, Forward)]

    def from(fromNode: Forward, msg: String): this.type = {

      fromNode.arrowBuffer += Arrow.`~>`.OfText(Option(msg).filter(_.nonEmpty)) -> this
      this
    }

    override def toString: String = text
  }

  object Forward extends Local.Diverging.UpperSemilattice.Inspection[Forward] {

    object inspect extends (Forward => inspect)
    case class inspect(value: Forward) extends Result {

      override def inductions: Seq[(_Arrow, Forward.this.inspect)] = {

        value.arrowBuffer.toSeq.map { v =>
          v._1 -> inspect(v._2)
        }
      }
    }
  }
}
