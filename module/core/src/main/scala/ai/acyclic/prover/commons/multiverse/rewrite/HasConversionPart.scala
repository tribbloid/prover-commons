package ai.acyclic.prover.commons.multiverse.rewrite

import zio.=!=

import scala.language.implicitConversions

trait HasConversionPart extends HasConversion with HasConversionPart.Imp1 {

  trait ConversionStep[-T, +R] {
    def normalise(v: T): R
  }

  implicit def conversionStepFromPart[T, R](
      implicit
      p: ConversionPart[T, R]
  ): ConversionStep[T, R] =
    (v: T) => p.normalise(v)

  /**
    * can be chained, unlike Conversion
    *
    * prefer to search for missing parts in forward direction
    */
  type ConversionPart[-T, +R] = HasConversionPart.ConversionPart[T, R]

  infix type ?++>[-T, +R] = Conversion[T, R]

  trait PartiallyConverted {}

}

/**
  * TODO: implement the summonning mechanism here and test cases
  *
  * all the forward search summoning should use 2 stages:
  *
  *   1. summon a TypeClass from:
  *      - (Lemma A) ConversionPart[T, R]
  *      - (Lemma B) (T => R) where R <: [[PartiallyConverted]]
  *   2. summon instances of forwardSearchView from the TYpeClass
  *
  * chained conversion using both ConversionPart and PartiallyConverted should be tested
  *
  * the test matrix should cover all the following 3 cases:
  *
  *   - chaining Lemma A
  *   - chaining Lemma B
  *   - chaining both Lemma A and Lemma B
  *
  * Do NOT modify or remove existing test case
  */
object HasConversionPart {

  trait ConversionPart[-T, +R] extends Conversion[T, R] {}

  // ConversionStepLowPriority removed to prevent implicit divergence loop with generic T=>R functions
  // Users should use ConversionPart explicitly or provide specific wrappers.

  // Layered trait hierarchy for implicit priority
  // Shorter chains are preferred through linearization
  // this can be much shorter if Scala implicit search is less lame
  trait Imp4 {
    self: HasConversionPart =>

    implicit def forwardSearchView4[T, R1, R2, R3, R4, R5](v: T)(
        implicit
        p1: ConversionStep[T, R1],
        p2: ConversionStep[R1, R2],
        p3: ConversionStep[R2, R3],
        p4: ConversionStep[R3, R4],
        p5: ConversionStep[R4, R5]
    ): R5 = p5.normalise(p4.normalise(p3.normalise(p2.normalise(p1.normalise(v)))))
  }

  trait Imp3 extends Imp4 {
    self: HasConversionPart =>

    implicit def forwardSearchView3[T, R1, R2, R3, R4](v: T)(
        implicit
        p1: ConversionStep[T, R1],
        p2: ConversionStep[R1, R2],
        p3: ConversionStep[R2, R3],
        p4: ConversionStep[R3, R4]
    ): R4 = p4.normalise(p3.normalise(p2.normalise(p1.normalise(v))))
  }

  trait Imp2 extends Imp3 {
    self: HasConversionPart =>

    implicit def forwardSearchView2[T, R1, R2, R3](v: T)(
        implicit
        p1: ConversionStep[T, R1],
        p2: ConversionStep[R1, R2],
        p3: ConversionStep[R2, R3]
    ): R3 = p3.normalise(p2.normalise(p1.normalise(v)))
  }

  trait Imp1 extends Imp2 {
    self: HasConversionPart =>

    implicit def forwardSearchView[T, R, R2](v: T)(
        implicit
        left: ConversionStep[T, R],
        right: ConversionStep[R, R2]
    ): R2 = right.normalise(left.normalise(v))

    implicit def conversionStepFromPartiallyConverted[T, R <: PartiallyConverted](
        implicit
        fn: T => R,
        ev: T =!= R
    ): ConversionStep[T, R] =
      (v: T) => fn(v)
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
