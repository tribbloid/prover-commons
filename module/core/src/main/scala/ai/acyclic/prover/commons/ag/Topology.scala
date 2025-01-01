package ai.acyclic.prover.commons.ag

trait Topology { // constructor of axioms

  type _I <: Topology.AnyGraph._I

  case class _Template[V]() extends Foundation.Template {
    final type _I = Topology.this._I
    final type _V = V
  }

  def newTemplate[V]: _Template[V] = _Template[V]()

  type Node[V] = Foundation.Node[_I, V]
  type Setter[V] = Foundation.Setter[_I, V]
}

object Topology {

  object AnyGraph extends Topology {

    trait _I extends Foundation.Induction {

      override def arrow: Arrow
    }

    object Outbound extends Topology {

      trait _I extends AnyGraph._I {

        override def arrow: Arrow.Outbound
      }
    }
  }

  object Poset extends Topology {

    trait _I extends AnyGraph._I
  }

  object SemiLattice extends Topology {

    trait _I extends Poset._I

    object Upper extends Topology {

      trait _I extends SemiLattice._I with AnyGraph.Outbound._I
    }
  }

  object Tree extends Topology {

    trait _I extends SemiLattice.Upper._I

  }

  { // sanity

    val t0: Poset._Template[Any] = Poset.newTemplate[Any]
    val t1: Tree._Template[Int] = Tree.newTemplate[Int]

    implicitly[t1._V =:= Int]
    val k: t1._I = ??? : Tree._I

    implicitly[t1._I =:= Tree._I]
//    implicitly[t1._I =:= Tree._I]

    implicitly[t1._I <:< t0._I]
    implicitly[t1._V <:< t0._V]
    implicitly[t1._Node <:< t0._Node]
//    implicitly[t1._Setter <:< t1._Setter] NOT TRUE

    def node2Setter[I <: Topology.AnyGraph._I, V](v: Foundation.Node[I, V])(
        implicit
        ev: Foundation.T[I, V]
    ): ev._Setter = {

      ???
    }

    val n1: t1._Node = ???
    val setter: t1._Setter = {
      node2Setter[Tree._I, Int](n1)
      node2Setter(n1)
    }

  }

}
