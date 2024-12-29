package ai.acyclic.prover.commons.graph.local.ops

import ai.acyclic.prover.commons.graph.Refinement
import ai.acyclic.prover.commons.graph.local.{Local, LocalEngine}
import ai.acyclic.prover.commons.graph.topology.Induction

trait AnyGraphBinary extends Local.AnyGraph.Ops.Binary {

  override type Prev <: AnyGraphUnary

  case class Union[VV]()(
      implicit
      ev1: prev.arg.Value <:< VV,
      ev2: arg.Value <:< VV
  ) extends Plan_[Induction.AnyGraphT, VV] {

    type _Axiom = Induction.AnyGraphT

    override lazy val getEntries: Vector[Refinement.NodeK.Lt[Induction.AnyGraphT, VV]] = {

      val e1: Seq[prev.ArgNode] = prev.distinctEntries
      val e2: Seq[ArgNode] = {

        val ops: AnyGraphUnary.^[arg.type] = LocalEngine.graphAsUnary(arg)
        ops.distinctEntries
      }

      // the axiom bound can be tighten further
      val roots1: Seq[Refinement.NodeK.Lt[Induction.AnyGraphT, VV]] = e1.map(n => n.upcast[VV])
      val roots2: Seq[Refinement.NodeK.Lt[Induction.AnyGraphT, VV]] = e2.map(n => n.upcast[VV])

      (roots1 ++ roots2).distinct.toVector
    }
  }
}

object AnyGraphBinary {}
