package ai.acyclic.prover.commons.multiverse.rewrite

trait HasConversionPart {

  /**
    * can be chained, unlike Conversion
    *
    * prefer to search for missing parts in forward direction
    */
  trait ConversionPart[-T, +R] extends Conversion[T, R] {}

//  implicit def direct[T, R, R2](
//                                        implicit
//                                        left: Conversion[T, R],
//                                        right: ConversionPart[R, R2]
//                                      ): Conversion[T, R2] = {
//    ???
//  }

  implicit def forwardSearch[T, R, R2](
      implicit
      left: Conversion[T, R],
      right: ConversionPart[R, R2]
  ): Conversion[T, R2] = {
    ???
  }

}

object ConversionPart {

//  trait BackwardMixin[-T, +R] { self: Conversion[T, R] =>
//
//    implicit def backwardSearch[T0, R0 >: R](v: T0)(
//        implicit
//        prev: ConversionPart[T0, T]
//    ): R0 = {
//      normalise(prev.normalise(v))
//    }
//
//  }
}
