package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.*
import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.Hierarchy

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.util.control.Breaks

trait UpperSemilatticeUnary extends Local.Semilattice.Upper.Ops.Unary {

  def text_hierarchy(
      implicit
      format: Hierarchy
  ): format.Viz[arg.Value] = format.Viz(arg)

  lazy val maxNodeOpt: Option[arg.NodeV] = {

    val entryIDs = arg.entries

    if (entryIDs.isEmpty) None
    else if (entryIDs.size == 1) entryIDs.headOption
    else {
      val id_counters: mutable.Map[Any, (arg.NodeV, AtomicInteger)] = {

        mutable.Map(
          entryIDs.map { nn =>
            nn.identityKey -> (nn -> new AtomicInteger())
          }*
        )
      }

      Breaks.breakable {

        val unary = AnyGraphUnary.^(arg, maxDepth)

        unary
          .Traverse(
            down = { n =>
              val counterOpt = id_counters.get(n.identityKey)
              counterOpt.foreach {
                case (_, cc) =>
                  val ccv = cc.incrementAndGet()

                  if (ccv >= 2) id_counters.remove(n)

                  if (id_counters.size == 1) {
                    Breaks.break()
                  }
              }
            }
          )
          .DepthFirst
      }

      require(id_counters.size == 1, "NOT a semilattice!")

      id_counters.map(_._2._1).headOption
    }

  }

}

object UpperSemilatticeUnary {

  case class ^[A <: Local.Semilattice.Upper.Graph[?]](
      arg: A,
      override val maxDepth: Int = 20
  ) extends UpperSemilatticeUnary {

    override type Arg = A
  }
}
