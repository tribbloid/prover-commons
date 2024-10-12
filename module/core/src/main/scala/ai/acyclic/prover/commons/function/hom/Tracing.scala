package ai.acyclic.prover.commons.function.hom

// TODO: remove, tracing is built into Circuit
case class Tracing[I, O](left: Hom.Circuit[I, O]) {

  // define functions to make Scala for-comprehension possible here

  // before SIP-62 for comprehension improvement (https://docs.scala-lang.net/sips/better-fors.html)
  // map & withFilter

  // after
  // flatMap

}

object Tracing {}
