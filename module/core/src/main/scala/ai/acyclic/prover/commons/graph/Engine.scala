package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Conj, Lawful, Topology}

trait Engine {
  self: Singleton =>

  import ai.acyclic.prover.commons.graph.topology.Topology._
  import Engine._

  type Dataset[+T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait TheGraphK[+L <: Conj] extends GraphK[L] {
    override type _E = Engine.this.type
    final override def engine: _E = Engine.this

    override type Dataset[+v] = Engine.this.Dataset[v]

    def entriesC: Dataset[NodeK.Compat[L, Value]]

    lazy val entries: Dataset[NodeK.Compat[L, Value]] = {
      entriesC
    }
  }

  object TheGraphK {

    type Aux[+L <: Conj, V] = TheGraphK[L] { type Value = V }

    trait Impl[+L <: Conj, V] extends TheGraphK[L] {
      type Value = V
    }

    // Acronym of "Less Than"
    type Lt[+C <: Conj, +V] = Aux[C, _ <: V]

    /**
      * Graph representation without any validation
      */
    case class Unchecked[C <: Conj, V](
        entriesC: Dataset[NodeK.Compat[C, V]]
    )(
        implicit
        override val assuming: C
    ) extends TheGraphK.Impl[C, V] {

      // TODO: implement Lawful variant which summons corresponding Topology.Law and validate the graph
    }
  }

  trait PlanK[+L <: Conj] extends Lawful.Struct[L] {

    private[this] type OGraph = TheGraphK.Aux[L, Value]
//    type ONode = NodeKind.Lt[L, Value]

    def compute: OGraph

    final lazy val resolve: OGraph = compute

    lazy val assuming: L = resolve.assuming
  }

  object PlanK {

    type Aux[L <: Conj, V] = PlanK[L] { type Value = V }

    trait Impl[L <: Conj, V] extends PlanK[L] {
      type Value = V
    }

//    type Lt[L <: Law, +V] = PlanKind[_ <: L] { type Value <: V }

    implicit class LeafPlan[L <: Conj, V](
        override val compute: TheGraphK.Aux[L, V]
    ) extends PlanK[L] {

      override type Value = V
    }
  }

  trait TheLawful extends Lawful {

    type Graph[v] = TheGraphK.Aux[Conj_/\, v]

    type Plan[v] = PlanK.Aux[Conj_/\, v]

    trait PlanImpl[v] extends PlanK.Impl[Conj_/\, v]
  }

  object GraphBuilder {

    type Of[C <: Conj] = GraphBuilder[_] { type Conj_/\ <: C }

    def sanity[C <: Conj]: Unit = { // sanity

      val example: Of[C] = ???

      implicitly[example._Conj <:< example.Conj_/\]

      implicitly[example.Conj_/\ <:< C]

      implicitly[example._Conj <:< C]
    }
  }

  abstract class GraphBuilder[T <: Topology](val topology: T) extends TheLawful {

    type Conj_/\ = topology.Conj_/\

    type _Arrow <: Arrow

    type _Conj = Conj_/\ { type _Arrow = GraphBuilder.this._Arrow }

    protected lazy val _assuming: _Conj = Conj[_Conj]

    implicit def self: this.type = this

    trait StructMixin extends Lawful.Struct[Conj_/\] {

      final lazy val assuming = _assuming
    }

    trait NodeImpl[V] extends NodeK.Impl[Conj_/\, V] with StructMixin {}

    trait RewriterImpl[V] extends RewriterK.Impl[Conj_/\, V] with StructMixin {}

    def makeTightest[C <: Conj_/\, V](
        nodes: NodeK.Compat[C, V]*
    )(
        implicit
        tightest: GraphBuilder.Of[C]
    ): TheGraphK.Aux[C, V] =
      TheGraphK.Unchecked[C, V](parallelize(nodes))(tightest._assuming)

    def make[V](
        nodes: NodeK.Compat[Conj_/\, V]*
    ): Graph[V] = makeTightest[Conj_/\, V](nodes: _*)(_assuming)

    def apply[C <: Conj_/\, V]( // alias of makeTightest
        nodes: NodeK.Compat[C, V]*
    )(
        implicit
        tightest: GraphBuilder.Of[C]
    ): TheGraphK.Aux[C, V] = makeTightest[C, V](nodes: _*)

    def empty[V]: Graph[V] = make[V]()

    trait UntypedDef {
      self: Singleton =>

      trait UntypedNode extends NodeK.Untyped[Conj_/\] with StructMixin {
        self: UntypedDef.this.Node =>

        type Value = UntypedDef.this.Node
      }

      type Node <: UntypedNode

      type Graph = TheGraphK.Aux[Conj_/\, Node]
    }

    trait Ops extends HasMaxRecursionDepth {

      def outer = GraphBuilder.this

      // invariant type
      // like `Plan`
      //  all following types refers to the tightest bound of the actual graph structure

      // UNLIKE `Plan`
      //  implementation of `Ops` is subclass-compatible
      //  e.g. it is possible to create a `GraphUnary` from a Tree

      type Prev
      val prev: Prev

      type AcceptingLaw = GraphBuilder.this.Conj_/\
      type ArgLaw <: AcceptingLaw
      type ArgV

      object Arg extends TheLawful {

        type Conj_/\ = ArgLaw
      }

      type ArgPlan = Arg.Plan[ArgV]
      def argPlan: ArgPlan

      type Arg = Arg.Graph[ArgV]
      def arg: Arg = argPlan.resolve

      type ArgNode = Arg.Node[ArgV]
      type ArgRewriter = Arg.Rewriter[ArgV]
    }

    object Ops {

      type Aux[P <: PlanK[_]] = Ops { type InputPlan = P }

      trait Unary extends Ops {

        type Prev = Unit
        val prev: Unit = {}
      }

      trait Binary extends Ops {

        type Prev <: Unary
      }
    }
  }

  trait Syntax {

    object AnyGraph extends GraphBuilder(AnyGraphT) {

      override type _Arrow = Arrow

      object Outbound extends GraphBuilder(AnyGraphT.OutboundT) {

        override type _Arrow = Arrow.`~>`.^
      }
      type Outbound[V] = Outbound.Graph[V]

    }
    type AnyGraph[V] = AnyGraph.Graph[V]

    object Poset extends GraphBuilder(PosetT) {

      override type _Arrow = Arrow
    }
    type Poset[V] = Poset.Graph[V]

    object Semilattice extends GraphBuilder(SemilatticeT) {

      override type _Arrow = Arrow

      object Upper extends GraphBuilder(SemilatticeT.UpperT) {

        override type _Arrow = Arrow.`~>`.^
      }
      type Upper[V] = Upper.Graph[V]

    }
    type Semilattice[V] = Semilattice.Graph[V]

    object Tree extends GraphBuilder(TreeT) {

      override type _Arrow = Arrow.`~>`.^

      case class Singleton[V](value: V) extends NodeImpl[V] {

        final override lazy val inductionC = Nil
      }
    }
    type Tree[V] = Tree.Graph[V]
  }

}

object Engine {

  trait HasMaxRecursionDepth {

    def maxDepth: Int
  }
}
