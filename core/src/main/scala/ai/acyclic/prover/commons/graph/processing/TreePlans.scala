//package ai.acyclic.prover.commons.graph.processing
//
//import ai.acyclic.prover.commons.graph.local.Tree
//
//case class TreePlans[N](t: Tree[N]) {
//
//  import TreePlans._
//
//  // obviously, tree is a monad
//  case class FlatMap[N2](
//      fn: N => Seq[N2]
//  ) {
//
//    object DepthFirst extends _FlatMap[N, N2] {
//
//      case class NodeInMigration(node: N) {
//
//        lazy val after: Seq[N2] = fn(node)
//      }
//
//      override def exe: Tree[N] = {}
//    }
//  }
//}
//
//object TreePlans extends PlanType {
//
//  type OUB[N] = Tree[N]
//
//  trait _FlatMap[N, N2] extends Unary[N] {}
//}
