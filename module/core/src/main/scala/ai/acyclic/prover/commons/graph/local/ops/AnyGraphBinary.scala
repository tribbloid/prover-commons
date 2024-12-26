package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}

trait AnyGraphBinary extends Local.AnyGraph.Ops.Binary {

  override type Prev <: AnyGraphUnary

  case class Union[V]()(
      implicit
      ev1: prev.arg.Value <:< V,
      ev2: arg.Value <:< V
  ) extends LocalEngine._PlanK[arg._Axiom] {

    override lazy val entries: Vector[Node[V]] = {

      val e1: Seq[prev.arg.NodeV] = prev.distinctEntries
      val e2: Seq[arg.NodeV] = arg.distinctEntries.asInstanceOf[Seq[arg.NodeV]]

      val roots1: Seq[Node[V]] = e1
        .map { (n: prev.arg.NodeV) =>
          val ev1Fuck = ev1.asInstanceOf[n.Value <:< V]

          n.upcast[V](ev1Fuck).asInstanceOf[Node[V]]
        }
      val roots2: Seq[Node[V]] = e2
        .map { (n: arg.NodeV) =>
          val ev2Fuck = ev2.asInstanceOf[n.Value <:< V]

          n.upcast[V](ev2Fuck)
        }

      (roots1 ++ roots2).toVector
    }
  }
}

object AnyGraphBinary {}
