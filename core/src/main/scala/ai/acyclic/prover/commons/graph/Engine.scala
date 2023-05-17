package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.Same

trait Engine {
  self: Singleton =>

  import Topology._

  type Dataset[+T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait TheGraphK[+L <: Law] extends GraphK[L] {
    override type _E = Engine.this.type
    final override def engine: _E = Engine.this

    override type Dataset[+v] = Engine.this.Dataset[v]

    def entriesC: Dataset[NodeK.Compat[L, Value]]

    lazy val entries: Dataset[NodeK.Compat[L, Value]] = {
      entriesC
    }
  }

  object TheGraphK {

    type Aux[+L <: Law, V] = TheGraphK[L] { type Value = V }

    trait AuxEx[+L <: Law, V] extends TheGraphK[L] {
      type Value = V
    }

    // Acronym of "Less Than"
    type Lt[+C <: Law, +V] = Aux[C, _ <: V]

    /**
      * Graph representation without any validation
      */
    case class Unchecked[L <: Law, V](
        entriesC: Dataset[NodeK.Compat[L, V]]
    )(
        implicit
        override val law: L
    ) extends TheGraphK.AuxEx[L, V] {

      // TODO: implement Lawful variant which summons corresponding Topology.Law and validate the graph
    }
  }

  trait PlanK[+L <: Law] extends Lawful.Construct[L] {

    private[this] type OGraph = TheGraphK.Aux[L, Value]
//    type ONode = NodeKind.Lt[L, Value]

    def compute: OGraph

    final lazy val resolve: OGraph = compute

    lazy val law: L = resolve.law
  }

  object PlanK {

    type Aux[L <: Law, V] = PlanK[L] { type Value = V }

    trait AuxEx[L <: Law, V] extends PlanK[L] {
      type Value = V
    }

//    type Lt[L <: Law, +V] = PlanKind[_ <: L] { type Value <: V }

    implicit class LeafPlan[L <: Law, V](
        override val compute: TheGraphK.Aux[L, V]
    ) extends PlanK[L] {

      override type Value = V
    }
  }

  trait TheLawful extends Lawful {

    type GraphLike[v] = TheGraphK.Aux[_L, v]

    type Plan[v] = PlanK.Aux[_L, v]

    trait PlanEx[v] extends PlanK.AuxEx[_L, v]
  }

  abstract class GraphBuilder[T <: Topology](val topology: T) extends TheLawful {
    self: Singleton =>

    type _L = topology._L
    val law: _L

    trait NodeImpl[V] extends NodeK.AuxEx[_L, V] {

      final val law: GraphBuilder.this.law.type = GraphBuilder.this.law
    }

    trait RewriterImpl[V] extends RewriterK.AuxEx[_L, V] {

      final val law: GraphBuilder.this.law.type = GraphBuilder.this.law
    }

    def makeTightest[LL <: _L, V](
        nodes: NodeK.Compat[LL, V]*
    )(
        implicit
        tightestLaw: LL
    ): TheGraphK.Aux[LL, V] =
      TheGraphK.Unchecked(parallelize(nodes))

    def make[V](
        nodes: NodeK.Compat[_L, V]*
    ): GraphLike[V] =
      TheGraphK.Unchecked(parallelize(nodes))(law)

    def apply[V](
        nodes: NodeK.Compat[_L, V]*
    ): GraphLike[V] = make[V](nodes: _*)

    trait UntypedDef {
      self: Singleton =>

      trait UntypedNode extends NodeK.Untyped[_L] {
        self: UntypedDef.this.Node =>

        final val law: GraphBuilder.this.law.type = GraphBuilder.this.law

        type Value = UntypedDef.this.Node
      }

      type Node <: UntypedNode

      final type Graph = TheGraphK.Aux[_L, Node]
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

      object Arg extends TheLawful {

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

      type Aux[P <: PlanK[_]] = Ops { type InputPlan = P }
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

  trait Syntax {

    object Graph extends GraphBuilder(GraphT) {

      object Outbound extends GraphBuilder(GraphT.OutboundT) {
        object law extends _L with topology.LawImpl
      }
      type Outbound[V] = Outbound.GraphLike[V]

      object law extends _L with topology.LawImpl
    }
    type Graph[V] = Graph.GraphLike[V]

    object Poset extends GraphBuilder(PosetT) {
      object law extends _L with topology.LawImpl
    }
    type Poset[V] = Poset.GraphLike[V]

    object Semilattice extends GraphBuilder(SemilatticeT) {

      object Upper extends GraphBuilder(SemilatticeT.UpperT) {
        object law extends _L with topology.LawImpl
      }
      type Upper[V] = Upper.GraphLike[V]

      object law extends _L with topology.LawImpl
    }
    type Semilattice[V] = Semilattice.GraphLike[V]

    object Tree extends GraphBuilder(TreeT) {

      object law extends _L with topology.LawImpl
    }
    type Tree[V] = Tree.GraphLike[V]

  }

}

object Engine {}
