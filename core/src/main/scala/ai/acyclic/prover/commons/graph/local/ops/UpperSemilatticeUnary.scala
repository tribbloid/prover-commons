package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.Hierarchy

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.util.control.Breaks

trait UpperSemilatticeUnary extends Local.Semilattice.Upper.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.Semilattice.Upper._L]
  }

  // TODO: will trigger StackOverflow on cyclic graph or infinite acyclic graph
  //  maxDepth may need to be defined
  lazy val maxNodeOpt: Option[ArgNode] = {

    val entryIDs = arg.entries

    if (entryIDs.isEmpty) None
    else if (entryIDs.size == 1) entryIDs.headOption
    else {
      val id_counters: mutable.Map[Any, (ArgNode, AtomicInteger)] = {

        mutable.Map(
          entryIDs.map { nn =>
            nn.identityKey -> (nn -> new AtomicInteger())
          }: _*
        )
      }

      Breaks.breakable {

        arg
          .Traverse(
            down = { n =>
              val counterOpt = id_counters.get(n.identityKey)
              counterOpt.foreach {
                case (nn, cc) =>
                  val ccv = cc.incrementAndGet()

                  if (ccv >= 2) id_counters.remove(n)

                  if (id_counters.size == 1) {
                    Breaks.break()
                  }
              }

            }
          )
          .DepthFirst
          .resolve
      }

      require(id_counters.size == 1, "NOT a semilattice!")

      id_counters.map(_._2._1).headOption
    }

  }

  def diagram_hierarchy(
      implicit
      format: Hierarchy
  ): format.Viz[ArgV] = format.Viz(arg)
}

object UpperSemilatticeUnary {

  case class ^[L <: Local.Semilattice.Upper._L, V](argPlan: LocalEngine.PlanK.Aux[L, V])
      extends UpperSemilatticeUnary {

    override type ArgLaw = L

    override type ArgV = V
  }
}
