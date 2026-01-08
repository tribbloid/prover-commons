package ai.acyclic.prover.commons.function

trait Tuple2Fold[L, R] {
  // can be summoned to fold (T, Unit) or (Unit, T) into T

  type Out
}

object Tuple2Fold {

  case class FoldLeft[T]() extends Tuple2Fold[Unit, T] {
    type Out = T
  }

  case class FoldRight[T]() extends Tuple2Fold[Unit, T] {
    type Out = T
  }
}
