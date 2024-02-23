package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axiom, Lawful, Topology}

import scala.language.implicitConversions

trait Engine {
  self: Singleton =>

  import Axiom._
  import Engine._

  type Dataset[+T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait _GraphK[+X <: Axiom] extends GraphK[X] {
    override type _E = Engine.this.type
    final override def engine: _E = Engine.this

    override type Dataset[+V] = Engine.this.Dataset[V]

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

  trait Module {

    abstract class GraphCase[X <: Axiom, Y <: X](topology: Topology[X])(
        implicit
        val assuming: Y
    ) extends _Struct[X] {

      type _Axiom = X

      trait StructMixin extends _Struct[_Axiom] {

        final override type _Axiom = GraphCase.this._Axiom

        final lazy val assuming = GraphCase.this.assuming
      }

      /**
        * 1st API, most universal
        * @tparam V
        *   value tyupe
        */
      trait NodeImpl[V] extends NodeK.Impl[_Axiom, V] with StructMixin {

        def make: Graph[V] = makeExact[V](this)
      }

      /**
        * 2nd API, all [[Node]] under the same group can be connected to other [[Node]]
        */
      trait Group {

        trait INode extends NodeK.Untyped[_Axiom] with StructMixin {
          self: Group.this.Node =>

          type Value = Group.this.Node
        }

        type Node <: INode

        type Graph = _GraphK.Compat[_Axiom, Node]
      }

      /**
        * 3rd API, define a [[Node]] constructor that works on every [[V]]
        *
        * implicit function allows [[Node]] to act as an extension of [[V]]
        * @tparam V
        *   value type
        */
      trait Wiring[V] {

        trait INode extends NodeImpl[V]

        type Node <: NodeImpl[V]
        val Node: V => Node

        type Graph = _GraphK.Compat[_Axiom, V]

        implicit class ValuesOps(vs: IterableOnce[V]) {

          def make: Graph = {
            val nodes = vs.iterator.to(Seq).map(_.asNode)
            makeExact(nodes: _*)
          }
        }

        implicit class ValueOps(v: V) extends ValuesOps(Seq(v)) {

          def asNode: Node = Node(v)
        }

        Seq(1, 2).to(List)

        Array(1, 2).to(List)

      }

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

      trait Ops extends HasMaxRecursionDepth {

        def outer = GraphCase.this

        // invariant type
        // like `Plan`
        //  all following types refers to the tightest bound of the actual graph structure

        // UNLIKE `Plan`
        //  implementation of `Ops` is subclass-compatible
        //  e.g. it is possible to create a `GraphUnary` from a Tree

        type Prev
        val prev: Prev

        type AcceptingLaw = GraphCase.this._Axiom
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

    object AnyGraph extends GraphCase(AnyGraphT) {

      object Outbound extends GraphCase(AnyGraphT.OutboundT) {}
      type Outbound[V] = Outbound.Graph[V]

    }
    type AnyGraph[V] = AnyGraph.Graph[V]

    object Poset extends GraphCase(PosetT) {}
    type Poset[V] = Poset.Graph[V]

    object Semilattice extends GraphCase(SemilatticeT) {

      object Upper extends GraphCase(SemilatticeT.UpperT) {}
      type Upper[V] = Upper.Graph[V]

    }
    type Semilattice[V] = Semilattice.Graph[V]

    object Tree extends GraphCase(TreeT) {

      case class Singleton[V](value: V) extends NodeImpl[V] {

        final override lazy val getInduction = Nil
      }

      implicit class TreeNodeOps[V](n: NodeImpl[V]) {

        def mkTree: Tree[V] = Tree(n)
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
