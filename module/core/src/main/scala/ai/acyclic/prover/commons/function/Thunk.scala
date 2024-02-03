package ai.acyclic.prover.commons.function

import Impl._

case class Thunk[V](fn: Unit => V) extends (Unit :=> V) {

  lazy val value: Out = fn.apply((): Unit)

  override def apply(arg: Unit): Out = value
}
