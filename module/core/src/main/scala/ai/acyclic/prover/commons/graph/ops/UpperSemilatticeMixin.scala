package ai.acyclic.prover.commons.graph.ops

import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.topology.Topology
import ai.acyclic.prover.commons.graph.viz.Hierarchy

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.util.control.Breaks

trait UpperSemilatticeMixin {
  self: Engine & AnyGraphMixin =>

  case class UpperSemilattice1[
      X <: Topology.SemilatticeT.UpperT._Axiom,
      V
  ](
      override val arg: Graph.Lt[X, V],
      override val maxDepth: Int = 20
  ) extends Ops(arg)
      with Ops.Unary[X, V] {

    def asUpperSemilattice1Ops: this.type = this

    lazy val maxNodeOpt: Option[ArgNode] = {

      val entries = arg.entries
      val vs: Seq[Node[X, V]] = entries.collect // TODO: this is not efficient

      if (entries.isEmpty) None
      else if (vs.size == 1) vs.headOption
      else {
        val id_counters: mutable.Map[Any, (ArgNode, AtomicInteger)] = {

          mutable.Map(
            vs.map { nn =>
              nn.identity -> (nn -> new AtomicInteger())
            }*
          )
        }

        Breaks.breakable {

          val graphOps = AnyGraphOps1[X, V](arg, maxDepth)

          graphOps
            .Traverse(
              down = { n =>
                val counterOpt = id_counters.get(n.identity)
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

    //  def text_hierarchy(
    //      implicit
    //      format: Hierarchy
    //  ): format.Viz[arg.Value] = format.Viz(arg)
  }

  implicit def upperSemilattice1[
      X <: Topology.SemilatticeT.UpperT._Axiom,
      V
  ](
      arg: Graph.Lt[X, V]
  ): UpperSemilattice1[X, V] = UpperSemilattice1[X, V](arg)
}
