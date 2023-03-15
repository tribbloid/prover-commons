package ai.acyclic.prover.commons

trait EqualBy extends EqualBy.EqualByMixin {

  protected def _equalBy: Any
}

object EqualBy extends Same.Definition {

  override protected def _getID(v1: Any): Any = {
    v1 match {
      case v: EqualBy => v._equalBy
      case _          => v1
    }
  }
}
