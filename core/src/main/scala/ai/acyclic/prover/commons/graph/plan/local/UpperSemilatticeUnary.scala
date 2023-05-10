package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.Hierarchy

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.util.control.Breaks

trait UpperSemilatticeUnary extends Local.Semilattice.Upper.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.Semilattice.Upper._L]
  }

  // algorithm assumes no cycles
  lazy val maxNodeOpt: Option[ArgNode] = {

    val entries = arg.entries

    if (entries.isEmpty) None
    else if (entries.size == 1) entries.headOption
    else {
      val counters: mutable.Map[ArgNode, AtomicInteger] = {

        mutable.Map(
          entries.map { nn =>
            nn -> new AtomicInteger()
          }: _*
        )
      }

      Breaks.breakable {

        arg
          .Traverse(
            down = { n =>
              val counterOpt = counters.get(n)
              counterOpt.foreach { a =>
                a.incrementAndGet()

                if (a.get() >= 2) counters.remove(n)

                if (counters.size == 1) {
                  Breaks.break()
                }
              }

            }
          )
          .DepthFirst
          .resolve
      }

      val once = counters.toSeq.filter {
        case (_, v) => v.get() == 1
      }

      require(once.size == 1, "NOT a semilattice!")

      once.map(_._1).headOption
    }

  }

  def diagram_hierarchy(
      implicit
      format: Hierarchy
  ): format.Viz[ArgV] = format.Viz(arg)
}

object UpperSemilatticeUnary {

  case class ^[L <: Local.Semilattice.Upper._L, V](argPlan: LocalEngine.PlanKind.Aux[L, V])
      extends UpperSemilatticeUnary {

    override type ArgLaw = L

    override type ArgV = V
  }
}
