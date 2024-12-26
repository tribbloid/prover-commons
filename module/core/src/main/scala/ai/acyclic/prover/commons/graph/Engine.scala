package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Induction, Lawful, Topology}

import scala.language.implicitConversions

trait Engine {
  self: Singleton =>

  import Induction.*
  import Engine.*

  type Batch[+T]
  def parallelize[T](seq: Seq[T]): Batch[T]

  trait _Lawful extends Lawful {

    private type FB = _Axiom

//    type Plan[V] = PlanK[?] { type _Axiom <: FB; type Value <: V }
//    type Graph[V] = GraphKOfTheEngine[?] { type _Axiom <: FB; type Value <: V }
    type Graph[V] = _GraphK[FB] {

//      type _Axiom <: _Lawful.this._Axiom

      type Value <: V
    }

    //    type Plan[V] = PlanK.Lt[_Axiom, V]
    //
    //    trait PlanImpl[v] extends PlanK.Impl[_Axiom, v]
  }

  trait _GraphK[+X <: Induction] extends GraphK[X] with _Lawful { // TODO: will ber merged into GraphKofTheEngine

    override type Batch[+V] = Engine.this.Batch[V]

    override type _E = Engine.this.type
    final override lazy val engine: _E = Engine.this
  }

  trait _PlanK[X <: Induction] extends _GraphK[X] {

    override type _Axiom = X
  }

  object _GraphK {

    type Aux[X <: Induction, V] = _GraphK[X] { type Value = V }

    type Lt[X <: Induction, V] = _GraphK[? <: X] { type Value <: V }
    // Acronym of "Less Than"
//    type Compat[+X <: Induction, +V] = Aux[X, ? <: V]
  }

  trait Module {

    object SubEngine {

      def make[XX <: Induction, V](
          nodes: NodeK.Lt[XX, V]*
      )(
          topology: Topology { type _Axiom = XX }
      ): _GraphK.Aux[XX, V] = {

        val subEngine = new SubEngine(topology)
        subEngine.makeExact(nodes*)
      }

      implicit def asTopology[T <: Topology](v: SubEngine[T]): T = v.topology
    }

    class SubEngine[T <: Topology](
        val topology: T // this is a phantom object only used to infer type parameters
    ) extends _Lawful {

      type _Axiom = topology._Axiom
//      type _Arrow = topology._Arrow

      /**
        * Graph representation without any validation
        */
      case class Unchecked[V](
          entries: Batch[NodeK.Lt[_Axiom, V]]
      ) extends _GraphK[_Axiom]
          with topology.Element {

        type Value = V
      }

      def makeExact[V](
          nodes: NodeK.Lt[_Axiom, V]*
      ): Unchecked[V] = {
        val entries = parallelize(nodes)

        Unchecked[V](entries)
      }

      def makeTightest[XX <: _Axiom, V](
          nodes: NodeK.Lt[XX, V]*
      )(
          implicit
          topology: Topology { type _Axiom = XX }
      ): _GraphK.Aux[XX, V] = {

        SubEngine.make[XX, V](nodes*)(topology)
      }

//      def apply[XX <: _Axiom, V]( // alias of makeTightest
//          nodes: NodeK.Compat[XX, V]*
//      ): GraphKOfTheEngine.Aux[XX, V] = makeTightest[XX, V](nodes*)

      def empty[V]: Graph[V] = makeExact[V]()

      trait Ops extends HasMaxRecursionDepth {

        // UNLIKE `Plan`
        //  implementation of `Ops` is subclass-compatible
        //  e.g. it is possible to create a `GraphUnary` from a Tree

        type Prev
        val prev: Prev

        type Arg <: Graph[?]
        val arg: Arg
      }

      object Ops {

//        type Aux[P <: PlanK[?]] = Ops { type InputPlan = P }

        trait Unary extends Ops {

          type Prev = Unit
          val prev: Unit = {}

          // TODO: enable after X type arg is removed
//          trait NoChangePlan[X <: Induction] extends Engine.this._GraphK[X] {
//
//            type _Axiom = arg._Axiom
//          }
        }

        trait Binary extends Ops {

          type Prev <: Unary
        }
      }
    }

    object AnyGraph extends SubEngine(AnyGraphT) {

      object Outbound extends SubEngine(AnyGraphT.OutboundT) {}
      type Outbound[V] = Outbound.Graph[V]
    }
    type AnyGraph[V] = AnyGraph.Graph[V]

    object Poset extends SubEngine(PosetT) {}
    type Poset[V] = Poset.Graph[V]

    object Semilattice extends SubEngine(SemilatticeT) {

      object Upper extends SubEngine(SemilatticeT.UpperT) {}
      type Upper[V] = Upper.Graph[V]

    }
    type Semilattice[V] = Semilattice.Graph[V]

    object Tree extends SubEngine(TreeT) {

      implicit class TreeNodeOps[V](n: topology.Node[V]) {

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
