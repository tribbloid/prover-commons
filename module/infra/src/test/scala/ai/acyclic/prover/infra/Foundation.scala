package ai.acyclic.prover.infra

object Foundation {

  trait Arrow
  object Arrow {
    trait Outbound extends Arrow
  }

  trait NodeKOrGraphK {}

  trait NodeK extends NodeKOrGraphK {

    type FBound <: Induction

    protected def getInduction: Seq[FBound]
  }

  trait Induction {
    def arrow: Arrow
    def node: NodeK
  }

  object Induction {

    trait FP[+N <: NodeK] extends Induction { // short for "fixed point"
      def node: N
    }
  }

  trait GraphK extends NodeKOrGraphK {

    type Batch[+T] <: Iterable[T]

    type _Node <: NodeK

    def entries: Batch[_Node]
  }

  trait Topology {

    type FP = Induction.FP[Node]
    type FBound <: FP // doesn't work on Scala 3: https://github.com/scala/scala3/issues/22257

    type Node = NodeK { type FBound <: Topology.this.FBound }
    trait Node_ extends NodeK {
      type FBound = Topology.this.FBound
    }

    type Graph = GraphK { type _Node <: Node }
  }

  object Outbound extends Topology {

    trait FBound extends FP {
      def arrow: Arrow.Outbound
    }
  }

  object Poset extends Topology {

    trait FBound extends FP {}
  }

  object Semilattice extends Topology {

    trait FBound extends FP with Poset.FBound

    object Upper extends Topology {

      trait FBound extends FP with Semilattice.FBound with Outbound.FBound
    }
  }

  object Tree extends Topology {

    trait FBound extends FP with Semilattice.Upper.FBound {}
  }

  {
    // sanity

    object Node1 extends Tree.Node_ {

      override protected def getInduction: Seq[Tree.FBound] = {
        Seq(
          new Tree.FBound {
            override def arrow: Arrow.Outbound = ???

            override def node: Tree.Node = ???
          }
        )
      }
    }

    implicitly[Semilattice.Upper.Node <:< Semilattice.Node]
    implicitly[Semilattice.Upper.Node <:< Outbound.Node]

    implicitly[Semilattice.Upper.Graph <:< Semilattice.Graph]
    implicitly[Semilattice.Upper.Graph <:< Outbound.Graph]
  }
}
