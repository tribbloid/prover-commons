package ai.acyclic.prover.commons.multiverse.rewrite

/**
  * can be chained, unlike Conversion
  *
  * prefer to search for missing parts in forward direction
  */
trait ConversionPart[-T, +R] extends Conversion[T, R] with ConversionPart.BackwardMixin[T, R] {

  implicit def self: ConversionPart[T, R] = this

  implicit def forwardSearch[R2](v: T)(
      implicit
      next: ConversionPart[R, R2]
  ): R2 = {
    next.normalise(normalise(v))
  }
}

object ConversionPart {

  trait BackwardMixin[-T, +R] { self: Conversion[T, R] =>

    implicit def backwardSearch[T0, R0 >: R](v: T0)(
        implicit
        prev: ConversionPart[T0, T]
    ): R0 = {
      normalise(prev.normalise(v))
    }

  }
  // TODO: current Scala implicit conversion search is too weak to chain summoned implicits

}
