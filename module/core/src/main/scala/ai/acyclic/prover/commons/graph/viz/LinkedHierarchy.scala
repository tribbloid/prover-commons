package ai.acyclic.prover.commons.graph.viz

import ai.acyclic.prover.commons.graph.local.Local

object LinkedHierarchy {

  class Default(
      val backbone: Hierarchy
  ) extends LinkedHierarchy { // TODO: remove, identical to supertype

    override def maxRecursionDepth: Int = backbone.maxRecursionDepth
  }

  object Default extends Default(Hierarchy.Default)

//  implicit def newGroup: Default.RefGroup = Default.RefGroup()

}

abstract class LinkedHierarchy extends Visualisation.Local(Local.Diverging.Graph) {

  def __sanity[T](): Unit = {

    implicitly[MaxGraph[T] =:= Local.Diverging.Graph[T]]
    implicitly[MaxNode[T] =:= Local.Diverging.Graph.Node[T]]
  }

  def backbone: Hierarchy

  final override def show[V](data: MaxGraph[V]): Visual = Viz(data)

  case class Viz(override val unbox: MaxGraph[?]) extends Visual {

    val refGroup: RefGroup = RefGroup()

    private lazy val refPoset: Local.Diverging.Poset.Graph[refGroup.RefDomain.node] = {
      refGroup.build(unbox)
    }

    override lazy val text: String = {

      backbone.show(refPoset).text
    }
  }

}
