package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.local.Local

object LinkedHierarchy {

  case class Default(
      backbone: Hierarchy = Hierarchy.Default,
      override val getRefGroup: () => RefGroup = () => RefGroup()
  ) extends LinkedHierarchy { // TODO: remove, identical to supertype

    override def maxRecursionDepth: Int = backbone.maxRecursionDepth
  }

  object Default extends Default(Hierarchy.Default, () => RefGroup())
}

abstract class LinkedHierarchy extends Visualisation.Local(Local.Diverging.Graph) {

  def __sanity[T](): Unit = {

    implicitly[MaxGraph[T] =:= Local.Diverging.Graph[T]]
    implicitly[MaxNode[T] =:= Local.Diverging.Graph.Node[T]]
  }

  val backbone: Hierarchy
  val getRefGroup: () => RefGroup

  final override def show[V](data: MaxGraph[V]): IVisual = Viz(data)

  case class Viz(
      override val unbox: MaxGraph[?],
      refGroup: RefGroup = getRefGroup()
  ) extends IVisual {

    lazy val refPoset: Local.Diverging.Poset.Graph[refGroup.RefDomain.node] = {
      refGroup.convertGraph(unbox)
    }

    override lazy val text: String = {

      backbone.show(refPoset).text
    }
  }

}
