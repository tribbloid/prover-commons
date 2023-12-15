package ai.acyclic.prover.commons.function.draft

import ai.acyclic.prover.commons.function.FnLike


trait Fn0[+R] extends Function0[R] with FnLike {

  def get(): R

  def apply(): R = get()
}

object Fn0 {

  import FnLike._

  type Pure[+R] = Fn0[R] with PureTag

  trait Cached[+R] extends Fn0[R] with PureTag {

    lazy val value: R = get()

    final override def apply(): R = value
  }

  case class AsFn1[+R](fn: Fn0[R]) extends Fn1[Any, R] {

    override def apply(v1: Any): R = fn()

    override def toString: String = "(expanded from) " + fn.toString
  }

  def box1[R](fn: () => R): Fn0[R] = {
    fn match {
      case _: Fn0[_] => fn.asInstanceOf[Fn0[R]]
      case _         => () => fn()
    }
  }
}
