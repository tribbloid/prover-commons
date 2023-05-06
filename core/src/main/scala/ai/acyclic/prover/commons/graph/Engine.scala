package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.Same

trait Engine {
  self: Singleton =>

  import Topology._

  type Dataset[+T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait _GraphKind[+L <: Law] extends GraphKind[L] {
    override type _E = Engine.this.type
    final override def engine: _E = Engine.this

    override type Dataset[v] = Engine.this.Dataset[v]

    def entriesC: Dataset[NodeKind.Compat[L, Value]]

    lazy val entries: Dataset[NodeKind.Compat[L, Value]] = {
      entriesC
    }
  }

  object _GraphKind {

    type Aux[+L <: Law, V] = _GraphKind[L] { type Value = V }

    trait AuxEx[+L <: Law, V] extends _GraphKind[L] {
      type Value = V
    }

    // Acronym of "Less Than"
    type Lt[+C <: Law, +V] = Aux[C, _ <: V]

    /**
      * Graph representation without any validation
      */
    case class Unchecked[L <: Law, V](
        entriesC: Dataset[NodeKind.Compat[L, V]],
        override val nodeSameness: Same.Definition = Same.ByEquality
    )(
        implicit
        override val law: L
    ) extends _GraphKind.AuxEx[L, V] {
      // TODO: implement Lawful variant which summons corresponding Topology.Law and validate the graph
    }
  }

  trait PlanKind[+L <: Law] extends Lawful.Construct[L] {

    private[this] type OGraph = _GraphKind.Aux[L, Value]
//    type ONode = NodeKind.Lt[L, Value]

    def compute: OGraph

    final lazy val resolve: OGraph = compute

    lazy val law: L = resolve.law
  }

  object PlanKind {

    type Aux[L <: Law, V] = PlanKind[L] { type Value = V }

    trait AuxEx[L <: Law, V] extends PlanKind[L] {
      type Value = V
    }

//    type Lt[L <: Law, +V] = PlanKind[_ <: L] { type Value <: V }

    implicit class LeafPlan[L <: Law, V](
        override val compute: _GraphKind.Aux[L, V]
    ) extends PlanKind[L] {

      override type Value = V
    }
  }

  trait _Lawful extends Lawful {

    type GraphLike[v] = _GraphKind.Aux[_L, v]

    type Plan[v] = PlanKind.Aux[_L, v]

    trait PlanEx[v] extends PlanKind.AuxEx[_L, v]
  }

  abstract class GraphBuilder[T <: Topology](val topology: T) extends _Lawful {
    self: Singleton =>

    type _L = topology._L
    final val law = topology.law

    def makeTightest[LL <: _L, V](
        nodes: NodeKind.Compat[LL, V]*
    )(
        implicit
        tightestLaw: LL
    ): _GraphKind.Aux[LL, V] =
      _GraphKind.Unchecked(parallelize(nodes))

    def make[V](
        nodes: NodeKind.Compat[_L, V]*
    ): GraphLike[V] =
      _GraphKind.Unchecked(parallelize(nodes))(law)

    def apply[V](
        nodes: NodeKind.Compat[_L, V]*
    ): GraphLike[V] = make[V](nodes: _*)

    trait UntypedDef {
      self: Singleton =>

      trait UntypedNode extends NodeKind.Untyped[_L] {
        self: UntypedDef.this.Node =>

        type Value = UntypedDef.this.Node
      }

      type Node <: UntypedNode

      final type Graph = _GraphKind.Aux[_L, Node]
    }

    trait Ops {

      def outer = GraphBuilder.this

      // invariant type
      // like `Plan`
      //  all following types refers to the tightest bound of the actual graph structure

      // UNLIKE `Plan`
      //  implementation of `Ops` is subclass-compatible
      //  e.g. it is possible to create a `GraphUnary` from a Tree

      type Prev
      val prev: Prev

      type AcceptingLaw = GraphBuilder.this._L
      type ArgLaw <: AcceptingLaw
      type ArgV

      object Arg extends _Lawful {

        type _L = ArgLaw
      }

      type ArgPlan = Arg.Plan[ArgV]
      def argPlan: ArgPlan

      type Arg = Arg.GraphLike[ArgV]
      def arg: Arg = argPlan.resolve

      type ArgNode = Arg.Node[ArgV]
      type ArgRewriter = Arg.Rewriter[ArgV]
    }

    object Ops {

      type Aux[P <: PlanKind[_]] = Ops { type InputPlan = P }
      //  trait AuxEx[P <: Plan] extends Ops { type Input = P }

      trait Unary extends Ops {

        type Prev = Unit
        val prev: Unit = {}
      }

      trait Binary extends Ops {

        type Prev <: Unary
      }

      //      private def compileTimeCheck[V](): Unit = {}
    }
  }

//  trait PlanBuilder extends _Lawful {
//
//    trait PlanImpl[V] extends PlanKind.AuxEx[_L, V]
//  }

  trait Syntax {

    object Graph extends GraphBuilder(GraphT) {

      object Outbound extends GraphBuilder(GraphT.OutboundT) {
//        override val law: _L = new _L {}
      }
      type Outbound[V] = Outbound.GraphLike[V]

//      override val law: _L = new _L {}
    }
    type Graph[V] = Graph.GraphLike[V]

    object Poset extends GraphBuilder(PosetT) {
//      override val law: _L = new _L {}
    }
    type Poset[V] = Poset.GraphLike[V]

    object Semilattice extends GraphBuilder(SemilatticeT) {

      object Upper extends GraphBuilder(SemilatticeT.UpperT) {
//        override val law: _L = new _L {}
      }
      type Upper[V] = Upper.GraphLike[V]

//      override val law: _L = new _L {}
    }
    type Semilattice[V] = Semilattice.GraphLike[V]

    object Tree extends GraphBuilder(TreeT) {

//      override val law: _L = new _L {}
    }
    type Tree[V] = Tree.GraphLike[V]

  }

}

object Engine {}
