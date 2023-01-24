package ai.acyclic.prover.commons

trait HasOuter {

  def outer: AnyRef
}

object HasOuter {

  def outerListOf(v: Any): List[Any] = {

    val self = List(v)

    val outers = v match {
      case vv: HasOuter =>
        val outer = vv.outer
        outerListOf(outer)
      case _ =>
        Nil
    }

    self ++ outers
  }
}
