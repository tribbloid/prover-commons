package ai.acyclic.prover.commons.function.unused

case class Cached[R](fn0: () => R) {

  lazy val get: R = fn0()
}
