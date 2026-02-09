package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom.Poly

import scala.language.implicitConversions

trait SchemaMixin {
  self: BTuples =>

  /**
    * can convert a [[Inductive]] to a flat Scala tuple or Unit or value and back
    *
    * e.g.
    *   - (A, B) <-> A ><: B ><: Empty
    *   - (A) <-> A ><: Empty
    *   - A -> A ><: Empty
    *   - Unit -> Empty
    */
  object ToFlatRepr extends Poly {}

  /**
    * The inverse of [[ToFlatRepr]]
    */
  object FromFlatRepr extends Poly {}

}

object SchemaMixin {}
