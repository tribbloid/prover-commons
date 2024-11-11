package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.Hierarchy

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.util.control.Breaks

trait UpperSemilatticeUnary extends Local.Semilattice.Upper.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.Semilattice.Upper._Axiom]
  }

  lazy val maxNodeOpt: Option[ArgNode] = {

    val entryIDs = arg.entries

    if (entryIDs.isEmpty) None
    else if (entryIDs.size == 1) entryIDs.headOption
    else {
      val id_counters: mutable.Map[Any, (ArgNode, AtomicInteger)] = {

        mutable.Map(
          entryIDs.map { nn =>
            nn.identityKey -> (nn -> new AtomicInteger())
          } *
        )
      }

      Breaks.breakable {

        val unary = AnyGraphUnary.^(arg.asPlan, maxDepth)

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
          .resolve

      }

      require(id_counters.size == 1, "NOT a semilattice!")

      id_counters.map(_._2._1).headOption
    }

  }

  def text_hierarchy(
      implicit
      format: Hierarchy
  ): format.Viz[ArgV] = format.Viz(arg)
}

object UpperSemilatticeUnary {

  case class ^[L <: Local.Semilattice.Upper._Axiom, V](
      argPlan: LocalEngine.PlanK.Compat[L, V],
      override val maxDepth: Int = 20
  ) extends UpperSemilatticeUnary {

    override type ArgLaw = L

    override type ArgV = V
  }
}
