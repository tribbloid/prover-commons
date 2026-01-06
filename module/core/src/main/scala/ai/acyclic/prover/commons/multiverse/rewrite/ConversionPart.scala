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

  // Automatically chains two ConversionPart instances
  // This enables implicit conversions like A -> C when you have A -> B and B -> C
  implicit def chain[T, R, R2](
      implicit
      first: ConversionPart[T, R],
      next: ConversionPart[R, R2]
  ): ConversionPart[T, R2] = new ConversionPart[T, R2] {

    override def normalise(v: T): R2 = next.normalise(first.normalise(v))
  }

  trait BackwardMixin[-T, +R] { self: Conversion[T, R] =>

    implicit def backwardSearch[T0, R0 >: R](v: T0)(
        implicit
        prev: ConversionPart[T0, T]
    ): R0 = {
      normalise(prev.normalise(v))
    }

  }
}
