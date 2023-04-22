//package ai.acyclic.prover.commons.graph
//
//trait UntypedNodeKind[+C <: Topology.Constraint, +A <: Arrow] {
//
//  protected def getInduction: Seq[(A, UntypedNodeKind[C, A])]
//  lazy val induction = getInduction
//
//  final lazy val discoverNodes: Seq[UntypedNodeKind[C, A]] = induction.map(_._2)
//}
//
//object UntypedNodeKind {
//
//  implicit class AsNodeKind[
//      C <: Topology.Constraint,
//      A <: Arrow,
//      SELF <: UntypedNodeKind[C, A]
//  ](self: SELF)
//      extends NodeKind[C, A] {
//
//    override type Value = SELF
//    override def value: SELF = self
//
//    override protected def getInduction: Seq[(A, NodeKind.Lt[C, A, SELF])] = {
//      self.getInduction.map {
//        case (a, n) =>
//          a -> AsNodeKind(n)
//      }
//    }
//  }
//}
