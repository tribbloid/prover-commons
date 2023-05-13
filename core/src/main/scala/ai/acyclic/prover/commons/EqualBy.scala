//package ai.acyclic.prover.commons
//
//trait EqualBy extends EqualBy.EqualByMixin {
//
//  protected def _equalBy: Any
//}
//
//object EqualBy extends Same.By {
//
//  override protected def _getID(v1: Any): Some[Any] = {
//    v1 match {
//      case v: EqualBy => Some(v._equalBy)
//      case _          => Some(v1)
//    }
//  }
//}
