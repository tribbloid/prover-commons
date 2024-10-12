package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Axioms, Lawful, Topology}

trait Engine {
  self: Singleton =>

  import Axioms._
  import Engine._

  type Dataset[+T]
  def parallelize[T](seq: Seq[T]): Dataset[T]

  trait NodeImpl[+X <: Axioms] extends NodeK[X] with NodeOrGraph[X] {
//
//    override def asGraph =
//      _GraphK.Unchecked[X, Value](parallelize(this))(axioms)
  }

  trait _GraphK[+X <: Axioms] extends GraphK[X] with NodeOrGraph[X] {

    override type _E = Engine.this.type
    final override def engine: _E = Engine.this

    override type Dataset[+V] = Engine.this.Dataset[V]

    def getEntries: Dataset[NodeK.Compat[X, Value]]

    lazy val entries: Dataset[NodeK.Compat[X, Value]] = {
      getEntries
    }

    final def asGraph: this.type = this
  }

  object _GraphK {

    type Aux[+X <: Axioms, V] = _GraphK[X] { type Value = V }

    // Acronym of "Less Than"
    type Compat[+X <: Axioms, +V] = Aux[X, _ <: V]

    /**
      * Graph representation without any validation
      */
    case class Unchecked[X <: Axioms, V](
        getEntries: Dataset[NodeK.Compat[X, V]]
    )(
        override val axioms: X
    ) extends _GraphK[X] {

      type Value = V
      // TODO: implement Lawful variant which summons corresponding Topology.Law and validate the graph
    }

//    case class
  }

  trait PlanK[+X <: Axioms] extends Lawful.Struct[X] {

    private[this] type OGraph = _GraphK.Aux[X, Value]
//    type ONode = NodeKind.Lt[L, Value]

    def compute: OGraph

    final lazy val resolve: OGraph = compute

    lazy val axioms: X = resolve.axioms
  }

  object PlanK {

    type Compat[+X <: Axioms, V] = PlanK[X] { type Value = V }

    trait Impl[X <: Axioms, V] extends PlanK[X] {
      type Value = V
    }

  }

  implicit class LeafPlan[X <: Axioms, V](
      override val compute: _GraphK.Aux[X, V]
  ) extends PlanK[X] {

    override type Value = V

    lazy val asPlan = this
  }

  trait _Lawful extends Lawful {

    type Graph[v] = _GraphK.Aux[_Axiom, v]

    type Plan[v] = PlanK.Compat[_Axiom, v]

    trait PlanImpl[v] extends PlanK.Impl[_Axiom, v]
  }

  trait _Struct[+X <: Axioms] extends Lawful.Struct[X] with _Lawful {}

  trait Module {

    abstract class GraphType[X <: Axioms, Y <: X](
        topology: Topology[X] // this is a phantom object only used to infer type parameters
    )(
        implicit
        val axioms: Y
    ) extends _Struct[X] {

      type _Axiom = X

      trait Element extends _Struct[_Axiom] {
        // TODO: I don't think this trait should exist, Node and Rewriter should be agnostic to engines (local or distributed)
        //  Rewriter in addition should compile into e-graph

        final override type _Axiom = GraphType.this._Axiom

        final lazy val axioms = GraphType.this.axioms
      }

      /**
        * 1st API, most universal
        * @tparam V
        *   value tyupe
        */
      trait NodeImpl[V] extends NodeK.Impl[_Axiom, V] with Element with NodeOrGraph[_Axiom] {

//        override def asGraph: Graph[V] = makeExact[V](this)
      }

      /**
        * 2nd API, all [[node]] under the same group can be connected to other [[node]]
        */
      trait Group {

        trait _Node extends NodeK.Untyped[_Axiom] with Element {
          self: Group.this.node =>

          type Value = Group.this.node
        }

        type node <: _Node

        type Graph = _GraphK.Aux[_Axiom, node]
      }

      /**
        * 3rd API, define a [[node]] constructor that works on every [[V]]
        *
        * implicit function allows [[node]] to act as an extension of [[V]]
        *
        * @tparam V
        *   value type
        */
      trait Inspection[V] {

        trait _Node extends NodeImpl[V]

        protected type node <: _Node
        val node: V => node

        type Graph = _GraphK.Aux[_Axiom, V]

        implicit class ValuesOps(vs: IterableOnce[V]) {

          def make: Graph = {
            val nodes = vs.iterator.to(Seq).map(_.asNode)
            makeExact(nodes: _*)
          }
        }

        implicit class ValueOps(v: V) extends ValuesOps(Seq(v)) {

          def asNode: node = node(v)
        }
      }

      trait RewriterImpl[V] extends RewriterK.Impl[_Axiom, V] with Element {}

      def makeTightestWithAxiom[XX <: _Axiom, V](
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
      ): Graph[V] = makeTightestWithAxiom[_Axiom, V](nodes: _*)(axioms)

      def apply[XX <: _Axiom, V]( // alias of makeTightest
          nodes: NodeK.Compat[XX, V]*
      )(
          implicit
          assuming: XX
      ): _GraphK.Aux[XX, V] = makeTightest[XX, V](nodes: _*)

      def empty[V]: Graph[V] = makeExact[V]()

      trait Ops extends HasMaxRecursionDepth {

        def outer = GraphType.this

        // invariant type
        // like `Plan`
        //  all following types refers to the tightest bound of the actual graph structure

        // UNLIKE `Plan`
        //  implementation of `Ops` is subclass-compatible
        //  e.g. it is possible to create a `GraphUnary` from a Tree

        type Prev
        val prev: Prev

        type AcceptingLaw = GraphType.this._Axiom
        type ArgLaw <: AcceptingLaw
        type ArgV

        object Arg extends _Lawful {

          type _Axiom = ArgLaw
        }

        type ArgPlan = Arg.Plan[ArgV]

        def argPlan: ArgPlan

        type Arg = Arg.Graph[ArgV]

        lazy val arg: Arg = argPlan.resolve

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

    object AnyGraph extends GraphType(AnyGraphT) {

      object Outbound extends GraphType(AnyGraphT.OutboundT) {}
      type Outbound[V] = Outbound.Graph[V]

    }
    type AnyGraph[V] = AnyGraph.Graph[V]

    object Poset extends GraphType(PosetT) {}
    type Poset[V] = Poset.Graph[V]

    object Semilattice extends GraphType(SemilatticeT) {

      object Upper extends GraphType(SemilatticeT.UpperT) {}
      type Upper[V] = Upper.Graph[V]

    }
    type Semilattice[V] = Semilattice.Graph[V]

    object Tree extends GraphType(TreeT) {

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
