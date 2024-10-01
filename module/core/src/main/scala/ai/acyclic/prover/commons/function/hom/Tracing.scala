package ai.acyclic.prover.commons.function.hom


case class Tracing[I, O](fn: I => O) {

  // define functions to make Scala for-comprehension possible here

  // before SIP-62 for comprehension improvement (https://docs.scala-lang.net/sips/better-fors.html)
  // map & withFilter

  // after
  // flatMap
}
