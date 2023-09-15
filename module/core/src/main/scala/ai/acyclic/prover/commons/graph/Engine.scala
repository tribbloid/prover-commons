package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axiom, Lawful, Topology}

trait Engine {
  self: Singleton =>

  import Axiom._
  import Engine._

  type Dataset[+T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait _GraphK[+X <: Axiom] extends GraphK[X] {
    override type _E = Engine.this.type
    final override def engine: _E = Engine.this

    override type Dataset[+v] = Engine.this.Dataset[v]

    def entriesC: Dataset[NodeK.Compat[X, Value]]

    lazy val entries: Dataset[NodeK.Compat[X, Value]] = {
      entriesC
    }
  }

  object _GraphK {

    type Aux[+X <: Axiom, V] = _GraphK[X] { type Value = V }

    trait Impl[+X <: Axiom, V] extends _GraphK[X] {
      type Value = V
    }

    // Acronym of "Less Than"
    type Lt[+X <: Axiom, +V] = Aux[X, _ <: V]

    /**
      * Graph representation without any validation
      */
    case class Unchecked[X <: Axiom, V](
        entriesC: Dataset[NodeK.Compat[X, V]]
    )(
        override val assuming: X
    ) extends _GraphK.Impl[X, V] {

      // TODO: implement Lawful variant which summons corresponding Topology.Law and validate the graph
    }
  }

  trait PlanK[+X <: Axiom] extends Lawful.Struct[X] {

    private[this] type OGraph = _GraphK.Aux[X, Value]
//    type ONode = NodeKind.Lt[L, Value]

    def compute: OGraph

    final lazy val resolve: OGraph = compute

    lazy val assuming: X = resolve.assuming
  }

  object PlanK {

    type Aux[X <: Axiom, V] = PlanK[X] { type Value = V }

    trait Impl[X <: Axiom, V] extends PlanK[X] {
      type Value = V
    }

//    type Lt[L <: Law, +V] = PlanKind[_ <: L] { type Value <: V }

    implicit class LeafPlan[X <: Axiom, V](
        override val compute: _GraphK.Aux[X, V]
    ) extends PlanK[X] {

      override type Value = V
    }
  }

  trait _Lawful extends Lawful {

    type Graph[v] = _GraphK.Aux[Axiom_/\, v]

    type Plan[v] = PlanK.Aux[Axiom_/\, v]

    trait PlanImpl[v] extends PlanK.Impl[Axiom_/\, v]
  }

  trait Syntax {

    // TODO: split into another class
    trait GraphBuilderLike[X <: Axiom] extends _Lawful {

      override type Axiom_/\ = X

      type _Arrow <: Arrow

      type _Axiom = Axiom_/\ { type _Arrow = GraphBuilderLike.this._Arrow }
      lazy val axiom: _Axiom = Axiom[_Axiom]

//      implicit def self: GraphBuilderLike[Axiom_/\] = this
    }

    object GraphBuilderLike {

//      type Of[X <: Axiom] = GraphBuilderLike[X]

    }

    abstract class GraphBuilder[X <: Axiom, T <: Topology.HasTopology[X]](val topology: Topology.HasTopology[X] with T)
        extends GraphBuilderLike[X] {

      trait StructMixin extends Lawful.Struct[Axiom_/\] {

        final lazy val assuming = axiom
      }

      trait NodeImpl[V] extends NodeK.Impl[Axiom_/\, V] with StructMixin {}

      trait RewriterImpl[V] extends RewriterK.Impl[Axiom_/\, V] with StructMixin {}

      def makeTightestWIthAxiom[XX <: Axiom_/\, V](
          nodes: NodeK.Compat[XX, V]*
      )(
          axiom: XX
      ): _GraphK.Unchecked[XX, V] =
        _GraphK.Unchecked[XX, V](parallelize(nodes))(axiom)

      def makeTightest[XX <: Axiom_/\, V](
          nodes: NodeK.Compat[XX, V]*
      )(
          implicit
          tightest: Topology[XX]
      ): _GraphK.Unchecked[XX, V] =
        _GraphK.Unchecked[XX, V](parallelize(nodes))(tightest.axiom)

      def makeExact[V](
          nodes: NodeK.Compat[Axiom_/\, V]*
      ): Graph[V] = makeTightestWIthAxiom[Axiom_/\, V](nodes: _*)(this.axiom)

      def apply[X <: Axiom_/\, V]( // alias of makeTightest
          nodes: NodeK.Compat[X, V]*
      )(
          implicit
          tightest: Topology[X]
      ): _GraphK.Aux[X, V] = makeTightest[X, V](nodes: _*)

      def empty[V]: Graph[V] = makeExact[V]()

      trait UntypedDef {
        self: Singleton =>

        trait UntypedNode extends NodeK.Untyped[Axiom_/\] with StructMixin {
          self: UntypedDef.this.Node =>

          type Value = UntypedDef.this.Node
        }

        type Node <: UntypedNode

        type Graph = _GraphK.Aux[Axiom_/\, Node]
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

        type AcceptingLaw = GraphBuilder.this.Axiom_/\
        type ArgLaw <: AcceptingLaw
        type ArgV

        object Arg extends _Lawful {

          type Axiom_/\ = ArgLaw
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

    private def sanity[C <: Axiom]: Unit = { // sanity

      val example: GraphBuilderLike[C] = ???

      implicitly[example._Axiom <:< example.Axiom_/\]
      implicitly[example.Axiom_/\ <:< C]
      implicitly[example._Axiom <:< C]
    }
  }
}

object Engine {

  trait HasMaxRecursionDepth {

    def maxDepth: Int
  }
}
