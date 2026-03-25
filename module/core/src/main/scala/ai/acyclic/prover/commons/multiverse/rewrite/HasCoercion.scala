package ai.acyclic.prover.commons.multiverse.rewrite

import scala.annotation.implicitNotFound
import scala.language.implicitConversions

trait HasCoercion extends HasConversion with HasCoercion.MidPrioritySteps with HasCoercion.Imp1 {
  import HasCoercion.=!=

  /**
    * similar to [[Conversion]], but can happen transitively (compiler automatically chain multiple of them)
    */
  trait Coercion[-T, +R] extends Conversion[T, R] {}

  infix type Coe[-T, +R] = Coercion[T, R]
  infix type <%<[-T, +R] = Coercion[T, R] // resembles <:<
  infix type >%>[+R, -T] = T <%< R // resembles >:>

  trait Step[-T, +R] {
    def normalise(v: T): R
  }

  implicit def _stepFromPart[T, R](
      implicit
      p: Coercion[T, R],
      ev: T =!= R
  ): Step[T, R] =
    (v: T) => p.normalise(v)

  implicit def subtypeCoercion[T, R](
      implicit
      ev: T <:< R
  ): Coercion[T, R] = (v: T) => ev(v)

  trait IsPartiallyCoerced[T]
  object IsPartiallyCoerced {
    implicit def witness[T <: Coerced]: IsPartiallyCoerced[T] = new IsPartiallyCoerced[T] {}
  }

  trait Coerced {}

}

/**
  * TODO: implement the summonning mechanism here and test cases
  *
  * all the forward search summoning should use 2 stages:
  *
  *   1. summon a TypeClass from:
  *      - (Lemma A) ConversionPart[T, R]
  *      - (Lemma B) (T => R) where R <: [[Coerced]]
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
object HasCoercion { // TODO: name should be Coercion or Coe.

  @implicitNotFound(msg = "Cannot prove that ${A} =!= ${B}.")
  type =!=[A, B] = zio.=!=[A, B]

  trait LowPrioritySteps {
    self: HasCoercion =>

    implicit def _stepFromPartiallyCoercedFn[T, R](
        implicit
        fn: T => R,
        ev: T =!= R,
        guard: IsPartiallyCoerced[R]
    ): Step[T, R] =
      (v: T) => fn(v)
  }

  trait MidPrioritySteps extends LowPrioritySteps {
    self: HasCoercion =>

    implicit def _stepFromPartiallyCoercedConv[T, R](
        implicit
        fn: Conversion[T, R],
        ev: T =!= R,
        guard: IsPartiallyCoerced[R]
    ): Step[T, R] =
      (v: T) => fn(v)

    implicit def coercionFromFn[T, R](
        implicit
        fn: T => R,
        guard: IsPartiallyCoerced[R]
    ): Coercion[T, R] = new Coercion[T, R] {
      override def normalise(v: T): R = fn(v)
    }
  }

  // Layered trait hierarchy for implicit priority
  // Shorter chains are preferred through linearization
  // this can be much shorter if Scala implicit search is less lame
  trait Imp4 {
    self: HasCoercion =>

    implicit def forwardSearchView4[T, R1, R2, R3, R4, R5](v: T)(
        implicit
        p1: Step[T, R1],
        p2: Step[R1, R2],
        p3: Step[R2, R3],
        p4: Step[R3, R4],
        p5: Step[R4, R5]
    ): R5 = p5.normalise(p4.normalise(p3.normalise(p2.normalise(p1.normalise(v)))))
  }

  trait Imp3 extends Imp4 {
    self: HasCoercion =>

    implicit def forwardSearchView3[T, R1, R2, R3, R4](v: T)(
        implicit
        p1: Step[T, R1],
        p2: Step[R1, R2],
        p3: Step[R2, R3],
        p4: Step[R3, R4]
    ): R4 = p4.normalise(p3.normalise(p2.normalise(p1.normalise(v))))
  }

  trait Imp2 extends Imp3 {
    self: HasCoercion =>

    implicit def forwardSearchView2[T, R1, R2, R3](v: T)(
        implicit
        p1: Step[T, R1],
        p2: Step[R1, R2],
        p3: Step[R2, R3]
    ): R3 = p3.normalise(p2.normalise(p1.normalise(v)))
  }

  trait Imp1 extends Imp2 {
    self: HasCoercion =>

    implicit def forwardSearchView[T, R, R2](v: T)(
        implicit
        left: Step[T, R],
        right: Step[R, R2]
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
