//package ai.acyclic.prover.commons.graph.plan.local
//
//import ai.acyclic.prover.commons.graph.local.{Graph, Local}
//import ai.acyclic.prover.commons.graph.plan.PlanArg
//
//trait GraphBinary extends PlanArg.Binary {
//
//  type Prev <: GraphUnary
//
//  type IV
//  override type LastInputG <: IGK[IV]
//
//  case class Union[VV]()(
//      implicit
//      ev1: prev.IV <:< VV,
//      ev2: IV <:< VV
//  ) extends To[IGK[VV]] {
//
//    override def compute: IGK[VV] = {
//
//      val roots1: Local.Dataset[Graph.Node[VV]] =
//        prev.inputG.entriesC.map((n: Graph.Node[prev.IV]) => n.upcast[VV])
//      val roots2: Local.Dataset[Graph.Node[VV]] =
//        inputG.entriesC.map((n: Graph.Node[IV]) => n.upcast[VV])
//
//      Graph.apply(
//        (roots1 ++ roots2): _*
//      )
//    }
//  }
//}
