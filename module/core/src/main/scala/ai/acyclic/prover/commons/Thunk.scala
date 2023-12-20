package ai.acyclic.prover.commons

case class Thunk[+V](fn: () => V) {

  lazy val value: V = fn()
}
