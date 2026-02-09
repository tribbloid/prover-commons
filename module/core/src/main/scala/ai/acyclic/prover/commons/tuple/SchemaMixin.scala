package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.jit.hom.Hom.Poly

import scala.language.{dynamics, implicitConversions}

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
  trait Schema {

    type Repr <: Inductive

    def toRuntimeList(v: Inductive): List[Any]
    def fromRuntimeList(l: List[Any]): Inductive
  }

  /**
    * can convert a [[Inductive]] to a flat Scala tuple or Unit or value and back
    *
    * e.g.
    *   - (A, B) <-> A ><: B ><: Empty
    *   - (A) <-> A ><: Empty
    *   - A -> A ><: Empty
    *   - Unit -> Empty
    */
  trait FlatSchema extends Schema {

    type FlatRepr <: Any // actually Product | Unit | Value

    def forward(v: Inductive): FlatRepr // this should never yield a Tuple1, it should be flattened to a single value
    def reverse(v: FlatRepr): Inductive
  }

  object FlatSchema {

    infix type ~>[X, Y] = FlatSchema { type Repr = X; type FlatRepr = Y }

    implicit final def from[X](
        implicit
        ev: X ~> ?
    ): ev.type = ev

    implicit final def to[Y](
        implicit
        ev: ? ~> Y
    ): ev.type = ev
  }
}

object SchemaMixin {}
