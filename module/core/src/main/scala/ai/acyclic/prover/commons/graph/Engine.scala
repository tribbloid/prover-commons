package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Induction, Topology}

trait Engine {
  self: Singleton =>

  import Induction.*
  import Engine.*

  type Batch[+T]
  def parallelize[T](seq: Seq[T]): Batch[T]

  trait GraphKOfTheEngine[+X <: AnyGraphT] extends Refinement.GraphK[X] {

    override type _E = Engine.this.type
    final override lazy val engine: _E = Engine.this

    override type Batch[+V] = Engine.this.Batch[V]

    def getEntries: Batch[Refinement.NodeK.Lt[_Axiom, Value]]

    lazy val entries: Batch[Refinement.NodeK.Lt[_Axiom, Value]] = {
      getEntries
    }
  }

  object GraphKOfTheEngine {

    type Aux[+X <: AnyGraphT, V] = GraphKOfTheEngine[X] { type Value = V }

    type Lt[+X <: AnyGraphT, +V] = GraphKOfTheEngine[X] { type Value <: V }

    /**
      * Graph representation without any validation
      */
    case class Unchecked[X <: AnyGraphT, V](
        getEntries: Batch[Refinement.NodeK.Lt[X, V]]
    )(
        override val axiom: X
    ) extends GraphKOfTheEngine[X] {

      type _Axiom = X

      type Value = V

      { // sanity

        implicitly[Unchecked[X, V] <:< GraphKOfTheEngine.Aux[X, V]]
        implicitly[Unchecked[X, V] <:< GraphKOfTheEngine.Lt[X, V]]

      }
      // TODO: implement Lawful variant which summons corresponding Topology.Law and validate the graph
    }
  }

  trait PlanK[+X <: AnyGraphT] extends GraphKOfTheEngine[X] {}

  object PlanK {

    type Aux[+X <: AnyGraphT, V] = PlanK[X] { type Value = V }
    trait Aux_[X <: AnyGraphT, V] extends PlanK[X] {
      type Value = V
    }

  }

  trait _Lawful extends Refinement.Lawful {

    override type _Axiom <: AnyGraphT

    type Graph[v] = GraphKOfTheEngine.Lt[_Axiom, v]

    type Plan_[v] = PlanK.Aux_[_Axiom, v]
  }

  trait _Structure[+X <: AnyGraphT] extends Refinement.Structure[X] {

    type Graph[v] = GraphKOfTheEngine.Aux[axiom.type, v]

    type Plan[v] = PlanK.Aux[axiom.type, v]
  }

  trait Module {

    sealed abstract class GraphTypeImpl[X <: AnyGraphT, Y <: X](
        topology: Topology[X] // this is a phantom object only used to infer type parameters
    )(
        implicit
        val axiom: Y
    ) extends _Lawful {

      type _Axiom = X

      trait Element extends _Structure[_Axiom] {
        // TODO: I don't think this trait should exist, Node and Rewriter should be agnostic to engines (local or distributed)
        //  Rewriter in addition should compile into e-graph

        type _Axiom = X

        final lazy val axiom: Y = GraphTypeImpl.this.axiom
      }

      /**
        * 1st API, most universal
        * @tparam V
        *   value tyupe
        */
      trait NodeImpl[V] extends Refinement.NodeK.Aux_[_Axiom, V] with Element {

//        override def asGraph: Graph[V] = makeExact[V](this)
      }

      /**
        * 2nd API, all [[node]] under the same group can be connected to other [[node]]
        */
      trait Group {

        trait NodeInGroup extends Refinement.NodeK.Untyped[_Axiom] with Element {
          self: Group.this.node =>

          type Value = Group.this.node
        }

        type node <: NodeInGroup

        type Graph = GraphKOfTheEngine.Aux[_Axiom, node]
      }

      /**
        * 3rd API, define a [[inspect]] constructor that works on every [[V]]
        *
        * implicit function allows [[inspect]] to act as an extension of [[V]]
        *
        * @tparam V
        *   value type
        */
      trait Inspection[V] {

        type _Node = NodeImpl[V]

//        type node <: _Node
        val inspect: V => _Node

        type Graph_ = GraphKOfTheEngine.Aux[_Axiom, V]

        implicit class ValuesOps(vs: IterableOnce[V]) {

          def make: Graph_ = {
            val nodes = vs.iterator.to(Seq).map(_.asNode)
            makeExact[V](nodes*)
          }
        }

        implicit class ValueOps(v: V) extends ValuesOps(Seq(v)) {

          def asNode: _Node = inspect(v)
        }
      }

      trait RewriterImpl[V] extends Refinement.RewriterK.Impl[_Axiom, V] with Element {}

      def makeWithAxioms[XX <: _Axiom, V](
          nodes: Refinement.NodeK.Lt[XX, V]*
      )(
          assuming: XX
      ): GraphKOfTheEngine.Unchecked[XX, V] =
        GraphKOfTheEngine.Unchecked[XX, V](parallelize(nodes))(assuming)

      def makeTightest[XX <: _Axiom, V](
          nodes: Refinement.NodeK.Lt[XX, V]*
      )(
          implicit
          assuming: XX
      ): GraphKOfTheEngine.Unchecked[XX, V] =
        makeWithAxioms[XX, V](nodes*)(assuming)

      def makeExact[V](
          nodes: Refinement.NodeK.Lt[_Axiom, V]*
      ): GraphKOfTheEngine.Unchecked[X, V] =
        makeWithAxioms[_Axiom, V](nodes*)(this.axiom)

      def apply[XX <: _Axiom, V]( // alias of makeTightest
          nodes: Refinement.NodeK.Lt[XX, V]*
      )(
          implicit
          assuming: XX
      ): GraphKOfTheEngine.Unchecked[XX, V] = makeTightest[XX, V](nodes*)

      def empty[V]: Graph[V] = makeExact[V]()

      trait Ops extends HasMaxRecursionDepth {

        val outer: GraphTypeImpl.this.type = GraphTypeImpl.this

        // invariant type
        // like `Plan`
        //  all following types refers to the tightest bound of the actual graph structure

        // UNLIKE `Plan`
        //  implementation of `Ops` is subclass-compatible
        //  e.g. it is possible to create a `GraphUnary` from a Tree

        type Prev
        val prev: Prev

        type Accepting = GraphTypeImpl.this._Axiom

        type Arg <: GraphKOfTheEngine[Accepting]
        val arg: Arg

        type ArgNode = Refinement.NodeK.Lt[arg._Axiom, arg.Value]
        type ArgRewriter = Refinement.RewriterK.Aux[arg._Axiom, arg.Value]

      }

      object Ops {

        type Aux[P <: PlanK[?]] = Ops { type InputPlan = P }

        trait Unary extends Ops {

          type Prev = Unit
          val prev: Unit = {}

          abstract class Plan_[V] extends outer.Plan_[V] {

            type _Axiom = arg._Axiom

            override val axiom = arg.axiom
          }
        }

        trait Binary extends Ops {

          type Prev <: Unary

          abstract class Plan_[XX <: Induction, V](
              implicit
              override val axiom: XX
          ) extends outer.Plan_[V] {

            type _Axiom = XX
          }
        }
      }
    }

    object AnyGraph extends GraphTypeImpl(AnyGraphT) {

      object Outbound extends GraphTypeImpl(AnyGraphT.OutboundT) {}
      type Outbound[V] = Outbound.Graph[V]

    }
    type AnyGraph[V] = AnyGraph.Graph[V]

    object Poset extends GraphTypeImpl(PosetT) {}
    type Poset[V] = Poset.Graph[V]

    object Semilattice extends GraphTypeImpl(SemilatticeT) {

      object Upper extends GraphTypeImpl(SemilatticeT.UpperT) {}
      type Upper[V] = Upper.Graph[V]

    }
    type Semilattice[V] = Semilattice.Graph[V]

    object Tree extends GraphTypeImpl(TreeT) {

      case class Singleton[V](value: V) extends NodeImpl[V] {

        final override lazy val inductions: collection.immutable.Nil.type = Nil
      }

      implicit class TreeNodeOps[V](n: NodeImpl[V]) {

        def mkTree: Tree[V] = Tree.makeExact[V](n)
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
