package ai.acyclic.prover.commons.graph.ops

import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.topology.Topology

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.util.control.Breaks

trait PosetMixin {
  self: Engine & AnyGraphMixin =>

  case class Poset1[
      X <: Topology.Poset._Axiom,
      V
  ](
      override val arg: Graph[X, V]
  ) extends Ops.Unary[X, V](arg) {

    def ops_poset: this.type = this

    lazy val maxNodes: LazyList[ArgNode] = {

      val entries = arg.entries
      val vs: Seq[Node[X, V]] = entries.collect // TODO: this is not efficient

      if (entries.isEmpty) LazyList.empty
      else if (vs.size == 1) vs.to(LazyList)
      else {
        val id_counters: mutable.Map[Any, (ArgNode, AtomicInteger)] = {

          mutable.Map(
            vs.map { nn =>
              nn.identity -> (nn -> new AtomicInteger())
            }*
          )
        }

        Breaks.breakable {

          val graphOps = AnyGraphOps1[X, V](arg)

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

        id_counters.map(_._2._1).to(LazyList)
      }
    }
  }

  implicit def imp_poset[
      X <: Topology.Poset._Axiom,
      V
  ](
      arg: Graph[X, V]
  ): Poset1[X, V] = Poset1[X, V](arg)
}
