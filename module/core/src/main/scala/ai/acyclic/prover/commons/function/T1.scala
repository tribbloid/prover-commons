package ai.acyclic.prover.commons.function

import shapeless.HNil

import scala.language.implicitConversions

object T1 extends TierHigher {

  import shapeless.::

  override val lower: T0.type = T0

  implicit def unboxDirectly[T](v: Args[T :: HNil]): T = v.self.head

  //  trait Fn[-T, +R] extends Function1[T, R] with Function[IUB, R] {
  //    def apply(v1: T): R = hForm(Args(v1 :: HNil))
  ////    def apply(): R = hForm(Args(HNil))
  //  }

  implicit class Fn1Ops[T <: IUB, R](self: Function[T, R]) {

    //    override def andThen[R2](g: R => R2): Fn1[T, R2] = {
    //      val ab: Fn1[T, R] = this
    //      val bc: Fn1[R, R2] = box1(g)
    //      AndThen(
    //        ab,
    //        bc
    //      )
    //    }

  }

}
