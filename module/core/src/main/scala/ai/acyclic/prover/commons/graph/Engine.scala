package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.topology.{Law, Lawful, Topology}

trait Engine {
  self: Singleton =>

  import Engine.*

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
        val tp: Topology[L]
    ) extends TheGraphK.AuxEx[L, V] {

      // TODO: implement Lawful variant which summons corresponding Topology.Law and validate the graph
    }
  }

  trait PlanK[+L <: Law] extends Lawful.Struct[L] {

//    override lazy val ev: Topology.Ev[_ <: L] = resolve.ev

    private[this] type OGraph = TheGraphK.Aux[L, Value]
//    type ONode = NodeKind.Lt[L, Value]

    def compute: OGraph

    final lazy val resolve: OGraph = compute
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

    type Graph[V] = TheGraphK.Aux[Law_/\, V]

    type Plan[V] = PlanK.Aux[Law_/\, V]

    trait PlanEx[V] extends PlanK.AuxEx[Law_/\, V]
  }

  class GraphBuilder[T <: Topology[?]](
      val tp: T
  ) extends TheLawful {

    override type Law_/\ = tp.Law_/\

    implicitly[tp.Law_/\ <:< Law]

    type L = tp.Law_/\

    implicitly[L <:< Law]

    implicit final def self: this.type = this

    trait Insider extends Lawful.Struct[L] {

//      override val ev: Topology.Ev[L] = GraphBuilder.this
    }

    trait NodeImpl[V] extends NodeK.AuxEx[L, V] with Insider {}

    trait RewriterImpl[V] extends RewriterK.AuxEx[L, V] with Insider {}

    def makeTightest[LL <: Law_/\, V](
        nodes: NodeK.Compat[LL, V]*
    )(
        implicit
        tightest: Topology[LL]
    ): TheGraphK.Aux[LL, V] =
      TheGraphK.Unchecked(parallelize(nodes))(tightest)

    def make[V](
        nodes: NodeK.Compat[L, V]*
    ): Graph[V] = makeTightest[L, V](nodes: _*)(tp)

    def apply[LL <: Law_/\, V](
        nodes: NodeK.Compat[LL, V]*
    )(
        implicit
        tightest: Topology[LL]
    ): TheGraphK.Aux[LL, V] = makeTightest[LL, V](nodes: _*)

    def empty[V]: Graph[V] = make[V]()

    trait UntypedDef {
      self: Singleton =>

      trait UntypedNode extends NodeK.Untyped[Law_/\] {
        self: UntypedDef.this.Node =>

        type Value = UntypedDef.this.Node
      }

      type Node <: UntypedNode

      type Graph = TheGraphK.Aux[Law_/\, Node]
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

      type AcceptingLaw = GraphBuilder.this.Law_/\
      type ArgLaw <: AcceptingLaw
      type ArgV

      object Arg extends TheLawful {

        type Law_/\ = ArgLaw
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
      //  trait AuxEx[P <: Plan] extends Ops { type Input = P }

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

//    { // sanity
//
//      import Topology.*
//      val t1 = ev[Topology.Tree]
//      implicitly[t1._Arrow <:< Arrow]
//      implicitly[t1._Arrow <:< Arrow.`~>`.^]
//    }
//
//    { // sanity
//
//      import Topology.*
//      val t1 = dummy1(Topology.Tree)
//      implicitly[t1._Arrow <:< Arrow]
//      implicitly[t1._Arrow <:< Arrow.`~>`.^]
//    }
//
//    { // sanity
//
//      import Topology.*
//      val t1 = new Dummy2(Topology.Tree)
//      implicitly[t1._Arrow <:< Arrow]
//      implicitly[t1._Arrow <:< Arrow.`~>`.^]
//    }

    object AnyGraph extends GraphBuilder(Topology.AnyGraph) {

      object Outbound extends GraphBuilder(Topology.AnyGraph.Outbound) {}
      type Outbound[V] = Outbound.Graph[V]
    }

    type AnyGraph[V] = AnyGraph.Graph[V]

    object Poset extends GraphBuilder(Topology.Poset) {}
    type Poset[V] = Poset.Graph[V]

    object Semilattice extends GraphBuilder(Topology.Semilattice) {

      object Upper extends GraphBuilder(Topology.Semilattice.Upper) {}
      type Upper[V] = Upper.Graph[V]

    }
    type Semilattice[V] = Semilattice.Graph[V]

    object Tree extends GraphBuilder(Topology.Tree) {

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
