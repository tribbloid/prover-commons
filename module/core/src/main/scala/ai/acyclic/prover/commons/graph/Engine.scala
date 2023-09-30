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

    type Compat[+X <: Axiom, V] = _GraphK[X] { type Value = V }

    trait Impl[+X <: Axiom, V] extends _GraphK[X] {
      type Value = V
    }

    // Acronym of "Less Than"
    type Lt[+X <: Axiom, +V] = Compat[X, _ <: V]

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

    private[this] type OGraph = _GraphK.Compat[X, Value]
//    type ONode = NodeKind.Lt[L, Value]

    def compute: OGraph

    final lazy val resolve: OGraph = compute

    lazy val assuming: X = resolve.assuming
  }

  object PlanK {

    type Compat[+X <: Axiom, V] = PlanK[X] { type Value = V }

    trait Impl[X <: Axiom, V] extends PlanK[X] {
      type Value = V
    }

//    type Lt[L <: Law, +V] = PlanKind[_ <: L] { type Value <: V }

    implicit class LeafPlan[X <: Axiom, V](
        override val compute: _GraphK.Compat[X, V]
    ) extends PlanK[X] {

      override type Value = V
    }
  }

  trait _Lawful extends Lawful {

    type Graph[v] = _GraphK.Compat[_Axiom, v]

    type Plan[v] = PlanK.Compat[_Axiom, v]

    trait PlanImpl[v] extends PlanK.Impl[_Axiom, v]
  }

  trait _Struct[+X <: Axiom] extends Lawful.Struct[X] with _Lawful {}

  trait Syntax {

    abstract class GraphBuilder[X <: Axiom, Y <: X](topology: Topology[X])(
        implicit
        val assuming: Y
    ) extends _Struct[X] {

      type _Axiom = X

      trait StructMixin extends _Struct[_Axiom] {

        final lazy val assuming = GraphBuilder.this.assuming
      }

      trait NodeImpl[V] extends NodeK.Impl[_Axiom, V] with StructMixin {}

      trait RewriterImpl[V] extends RewriterK.Impl[_Axiom, V] with StructMixin {}

      def makeTightestWIthAxiom[XX <: _Axiom, V](
          nodes: NodeK.Compat[XX, V]*
      )(
          assuming: XX
      ): _GraphK.Unchecked[XX, V] =
        _GraphK.Unchecked[XX, V](parallelize(nodes))(assuming)

      def makeTightest[XX <: _Axiom, V](
          nodes: NodeK.Compat[XX, V]*
      )(
          implicit
          assuming: XX
      ): _GraphK.Unchecked[XX, V] =
        _GraphK.Unchecked[XX, V](parallelize(nodes))(assuming)

      def makeExact[V](
          nodes: NodeK.Compat[_Axiom, V]*
      ): Graph[V] = makeTightestWIthAxiom[_Axiom, V](nodes: _*)(assuming)

      def apply[XX <: _Axiom, V]( // alias of makeTightest
          nodes: NodeK.Compat[XX, V]*
      )(
          implicit
          assuming: XX
      ): _GraphK.Compat[XX, V] = makeTightest[XX, V](nodes: _*)

      def empty[V]: Graph[V] = makeExact[V]()

      trait UntypedDef {
        self: Singleton =>

        trait UntypedNode extends NodeK.Untyped[_Axiom] with StructMixin {
          self: UntypedDef.this.Node =>

          type Value = UntypedDef.this.Node
        }

        type Node <: UntypedNode

        type Graph = _GraphK.Compat[_Axiom, Node]
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

        type AcceptingLaw = GraphBuilder.this._Axiom
        type ArgLaw <: AcceptingLaw
        type ArgV

        object Arg extends _Lawful {

          type _Axiom = ArgLaw
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

    object AnyGraph extends GraphBuilder(AnyGraphX) {

      object Forward extends GraphBuilder(NormalisedX.ForwardX) {}
      type Outbound[V] = Forward.Graph[V]

    }
    type AnyGraph[V] = AnyGraph.Graph[V]

    object Poset extends GraphBuilder(PosetX) {}
    type Poset[V] = Poset.Graph[V]

    object Semilattice extends GraphBuilder(SemilatticeX) {

      object Upper extends GraphBuilder(SemilatticeX.UpperX) {}
      type Upper[V] = Upper.Graph[V]

    }
    type Semilattice[V] = Semilattice.Graph[V]

    object Tree extends GraphBuilder(TreeX) {

      case class Singleton[V](value: V) extends NodeImpl[V] {

        final override lazy val inductionC = Nil
      }
    }
    type Tree[V] = Tree.Graph[V]

//    private def sanity[C <: Axiom]: Unit = { // sanity
//
//      val example: GraphBuilderLike[C] = ???
//
//      implicitly[example._Axiom <:< example.Axiom_/\]
//      implicitly[example.Axiom_/\ <:< C]
//      implicitly[example._Axiom <:< C]
//    }
  }
}

object Engine {

  trait HasMaxRecursionDepth {

    def maxDepth: Int
  }
}
