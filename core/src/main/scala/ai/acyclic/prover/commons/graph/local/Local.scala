package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.Engine
import ai.acyclic.prover.commons.graph.viz.{Hierarchy, LinkedHierarchy}

import java.util.concurrent.atomic.AtomicInteger
import scala.language.implicitConversions

object Local extends Engine {

  final type Dataset[+T] = Vector[T]
  def parallelize[T](seq: Seq[T]): Dataset[T] = seq.toVector

//  implicit class OutboundGraphView[IG <: Graph.Outbound[_]](val self: IG) {
//
//    def diagram_linkedHierarchy(
//        implicit
//        group: LinkedHierarchy#Group
//    ): group.Viz[self.Value] = group.Viz(self)
//  }
//
//  implicit class UpperSemilatticeView[IG <: Semilattice.Upper[_]](val self: IG) {
//
//    {
//      require(self.entries.size == 1, "NOT a semilattice!")
//    }
//
//    // algorithm assumes no cycles
//    lazy val maxNode: self.Node = {
//
//      val counters: Map[self.Node, AtomicInteger] = {
//
//        val ee: Vector[self.Node] = self.entries
//        Map(
//          ee.map { nn =>
//            nn -> new AtomicInteger()
//          }: _*
//        )
//      }
//      self.Traverse(
//        down = { n =>
//          counters.get(n.asInstanceOf).foreach(a => a.incrementAndGet())
//        }
//      )
//
//      val once = counters.toSeq.filter {
//        case (k, v) => v.get() == 1
//      }
//
//      require(once.size == 1, "NOT a semilattice!")
//
//      once.head._1
//    }
//
//    def diagram_hierarchy(
//        implicit
//        format: Hierarchy
//    ): format.Viz[self.Value] = format.Viz(self)
//  }
}
