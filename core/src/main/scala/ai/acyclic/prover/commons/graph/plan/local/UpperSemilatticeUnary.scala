package ai.acyclic.prover.commons.graph.plan.local

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.viz.Hierarchy

import java.util.concurrent.atomic.AtomicInteger

trait UpperSemilatticeUnary extends Local.Semilattice.Upper.Ops.Unary {

  {
    implicitly[ArgLaw <:< Local.Semilattice.Upper._L]
  }

  // algorithm assumes no cycles
  lazy val maxNode: ArgNode = {

    val counters: Map[ArgNode, AtomicInteger] = {

      val ee: Vector[ArgNode] = arg.entries
      Map(
        ee.map { nn =>
          nn -> new AtomicInteger()
        }: _*
      )
    }
    arg.Traverse(
      down = { n =>
        counters.get(n.asInstanceOf).foreach(a => a.incrementAndGet())
      }
    )

    val once = counters.toSeq.filter {
      case (k, v) => v.get() == 1
    }

    require(once.size == 1, "NOT a semilattice!")

    once.head._1
  }

  def diagram_hierarchy(
      implicit
      format: Hierarchy
  ): format.Viz[ArgV] = format.Viz(arg)
}

object UpperSemilatticeUnary {

  case class ^[L <: Local.Semilattice.Upper._L, V](argPlan: LocalEngine.PlanKind.Aux[L, V]) extends UpperSemilatticeUnary {

    override type ArgLaw = L

    override type ArgV = V
  }
}
