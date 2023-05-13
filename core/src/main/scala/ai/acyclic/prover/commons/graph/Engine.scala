package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.Same

trait Engine {
  self: Singleton =>

  import Topology._

  type Dataset[+T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait TheGraphKind[+L <: Law] extends GraphKind[L] {
    override type _E = Engine.this.type
    final override def engine: _E = Engine.this

    override type Dataset[+v] = Engine.this.Dataset[v]

    def entriesC: Dataset[NodeKind.Compat[L, Value]]

    lazy val entries: Dataset[NodeKind.Compat[L, Value]] = {
      entriesC
    }
  }

  object TheGraphKind {

    type Aux[+L <: Law, V] = TheGraphKind[L] { type Value = V }

    trait AuxEx[+L <: Law, V] extends TheGraphKind[L] {
      type Value = V
    }

    // Acronym of "Less Than"
    type Lt[+C <: Law, +V] = Aux[C, _ <: V]

    /**
      * Graph representation without any validation
      */
    case class Unchecked[L <: Law, V](
        entriesC: Dataset[NodeKind.Compat[L, V]]
    )(
        implicit
        override val law: L
    ) extends TheGraphKind.AuxEx[L, V] {

      // TODO: implement Lawful variant which summons corresponding Topology.Law and validate the graph
    }
  }

  trait PlanKind[+L <: Law] extends Lawful.ConstructKind[L] {

    private[this] type OGraph = TheGraphKind.Aux[L, Value]
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
        override val compute: TheGraphKind.Aux[L, V]
    ) extends PlanKind[L] {

      override type Value = V
    }
  }

  trait TheLawful extends Lawful {

    type GraphLike[v] = TheGraphKind.Aux[_L, v]

    type Plan[v] = PlanKind.Aux[_L, v]

    trait PlanEx[v] extends PlanKind.AuxEx[_L, v]
  }

  abstract class GraphBuilder[T <: Topology](val topology: T) extends TheLawful {
    self: Singleton =>

    type _L = topology._L
    val law: _L

    trait Node[V] extends NodeKind.AuxEx[_L, V] {

      final val law: GraphBuilder.this.law.type = GraphBuilder.this.law
    }

    trait Rewriter[V] extends RewriterKind.AuxEx[_L, V] {

      final val law: GraphBuilder.this.law.type = GraphBuilder.this.law
    }

    def makeTightest[LL <: _L, V](
        nodes: NodeKind.Compat[LL, V]*
    )(
        implicit
        tightestLaw: LL
    ): TheGraphKind.Aux[LL, V] =
      TheGraphKind.Unchecked(parallelize(nodes))

    def make[V](
        nodes: NodeKind.Compat[_L, V]*
    ): GraphLike[V] =
      TheGraphKind.Unchecked(parallelize(nodes))(law)

    def apply[V](
        nodes: NodeKind.Compat[_L, V]*
    ): GraphLike[V] = make[V](nodes: _*)

    trait UntypedDef {
      self: Singleton =>

      trait UntypedNode extends NodeKind.Untyped[_L] {
        self: UntypedDef.this.Node =>

        final val law: GraphBuilder.this.law.type = GraphBuilder.this.law

        type Value = UntypedDef.this.Node
      }

      type Node <: UntypedNode

      final type Graph = TheGraphKind.Aux[_L, Node]
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

      type ArgNode = Arg.NodeCompat[ArgV]
      type ArgRewriter = Arg.RewriterCompat[ArgV]
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
