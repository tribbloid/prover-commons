package ai.acyclic.prover.commons.function.draft

import ai.acyclic.prover.commons.function.FnLike

import scala.language.implicitConversions

trait Fn1[-T, +R] extends Function1[T, R] with FnLike {
  import Fn1._

  override def andThen[R2](g: R => R2): Fn1[T, R2] = {
    val ab: Fn1[T, R] = this
    val bc: Fn1[R, R2] = box1(g)
    AndThen(
      ab,
      bc
    )
  }
}

object Fn1 {
  import FnLike._

  type Pure[+R] = Fn0[R] with PureTag

  case class AndThen[-A, B, +C](
      ab: Fn1[A, B],
      bc: Fn1[B, C]
  ) extends Fn1[A, C] {
    override def apply(v: A): C = {
      bc.apply(ab(v))
    }
  }

  def box1[T, R](fn: T => R): Fn1[T, R] = {
    fn match {
      case _: Fn1[_, _] => fn.asInstanceOf[Fn1[T, R]]
      case _            => v => fn(v)
    }
  }

  implicit def box2[T1, T2, R](f: (T1, T2) => R): Fn1[(T1, T2), R] =
    (t: (T1, T2)) => f(t._1, t._2)
}
