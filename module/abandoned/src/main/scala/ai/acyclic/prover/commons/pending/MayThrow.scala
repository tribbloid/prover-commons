package ai.acyclic.prover.commons.pending

import ai.acyclic.prover.commons.pending.PendingEffect.<<

object MayThrow {

  case class ^[E <: Throwable]() extends PendingEffect

  def apply[E <: Throwable] = new ^[E]()

  implicit class _ops[T, E <: Throwable](self: (() => T) << ^[E]) {

    def asEither: Either[E, T] = {
      try {
        val v = self.asInstanceOf[() => T].apply()
        Right(v)
      } catch {
        case e: E => Left(e)
      }
    }
  }
}
