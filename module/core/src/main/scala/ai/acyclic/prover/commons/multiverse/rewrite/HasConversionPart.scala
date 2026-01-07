package ai.acyclic.prover.commons.multiverse.rewrite

import scala.language.implicitConversions

trait HasConversionPart extends HasConversion with HasConversionPart.Imp1 {

  /**
    * can be chained, unlike Conversion
    *
    * prefer to search for missing parts in forward direction
    */
  trait ConversionPart[-T, +R] extends Conversion[T, R] {}
}

object HasConversionPart {

  // Layered trait hierarchy for implicit priority
  // Shorter chains are preferred through linearization
  trait Imp4 {
    implicit def forwardSearchView4[T, R1, R2, R3, R4, R5](v: T)(
        implicit
        p1: HasConversionPart#ConversionPart[T, R1],
        p2: HasConversionPart#ConversionPart[R1, R2],
        p3: HasConversionPart#ConversionPart[R2, R3],
        p4: HasConversionPart#ConversionPart[R3, R4],
        p5: HasConversionPart#ConversionPart[R4, R5]
    ): R5 = p5.normalise(p4.normalise(p3.normalise(p2.normalise(p1.normalise(v)))))
  }

  trait Imp3 extends Imp4 {
    implicit def forwardSearchView3[T, R1, R2, R3, R4](v: T)(
        implicit
        p1: HasConversionPart#ConversionPart[T, R1],
        p2: HasConversionPart#ConversionPart[R1, R2],
        p3: HasConversionPart#ConversionPart[R2, R3],
        p4: HasConversionPart#ConversionPart[R3, R4]
    ): R4 = p4.normalise(p3.normalise(p2.normalise(p1.normalise(v))))
  }

  trait Imp2 extends Imp3 {
    implicit def forwardSearchView2[T, R1, R2, R3](v: T)(
        implicit
        p1: HasConversionPart#ConversionPart[T, R1],
        p2: HasConversionPart#ConversionPart[R1, R2],
        p3: HasConversionPart#ConversionPart[R2, R3]
    ): R3 = p3.normalise(p2.normalise(p1.normalise(v)))
  }

  trait Imp1 extends Imp2 {
    implicit def forwardSearchView[T, R, R2](v: T)(
        implicit
        left: HasConversionPart#ConversionPart[T, R],
        right: HasConversionPart#ConversionPart[R, R2]
    ): R2 = right.normalise(left.normalise(v))
  }

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
