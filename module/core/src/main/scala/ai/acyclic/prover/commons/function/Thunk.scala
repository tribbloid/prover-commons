package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.function.PreDef.:=>

case class Thunk[V](fn: Unit :=> V) extends PreDef.Fn[Unit, V] {

  lazy val value: V = fn(())

  override def apply(arg: Unit): V = value
}
