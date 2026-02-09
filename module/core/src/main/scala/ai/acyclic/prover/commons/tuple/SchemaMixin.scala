package ai.acyclic.prover.commons.tuple

import scala.language.implicitConversions

trait SchemaMixin {
  self: BTuples =>

  /**
    * contains compiled functions shared by all instances of the same type T
    *
    * these functions SHOULD NOT be defined under the type T itself, otherwise compiler will repeatedly look for
    * evidences to construct them whenever a new instance of T is created.
    *
    * For the same reason, construction/inference of Schema should be interned
    */
  trait Schema[Repr <: Inductive] {}

  /**
    * can convert a [[Inductive]] to a flat Scala tuple or Unit or value and back
    *
    * e.g.
    *   - (A, B) <-> A ><: B ><: Empty
    *   - (A) <-> A ><: Empty
    *   - A -> A ><: Empty
    *   - Unit -> Empty
    */
  trait FlatSchema[
      Repr <: Inductive,
      FlatRepr <: Any // actually Product | Unit | Value
  ] extends Schema[Repr] {

    def forward(v: Repr): FlatRepr // this should never yield a Tuple1, it should be flattened to a single value
    def reverse(v: FlatRepr): Repr
  }

  object FlatSchema {

    infix type <->[X <: Inductive, Y] = FlatSchema[X, Y]

//    infix type ~>[-X <: Inductive, +Y] = FlatSchema[? <: X, ? <: Y]
//    infix type <~[-Y <: Inductive, +X] = FlatSchema[? <: X, ? <: Y]

    implicit final def from[X <: Inductive](
        implicit
        ev: X <-> ?
    ): ev.type = ev

    implicit final def to[Y](
        implicit
        ev: ? <-> Y
    ): ev.type = ev

  }
}

object SchemaMixin {}
