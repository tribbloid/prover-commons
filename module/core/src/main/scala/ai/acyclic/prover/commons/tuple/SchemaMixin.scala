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

    type TupleRepr <: Tuples.Inductive

    /**
      * Convert to a boundless repr (which is far from being the most efficient)
      *
      * will be superseded by a type class to convert any Inductive to another
      */
    def toTuple(v: Inductive): TupleRepr
    def fromTuple(v: TupleRepr): Inductive

    type FlatRepr <: Any // actually Product | Unit | Value
    def toFlat(v: Inductive): FlatRepr // this should never yield a Tuple1, it should be flattened to a single value
    def fromFlat(v: FlatRepr): Inductive

    def toRuntimeList(v: Inductive): List[Any]
    def fromRuntimeList(l: List[Any]): Inductive
  }

  object Schema {
    type Of[T] = Schema { type Repr = T }

    type FromTuple[T <: Tuples.Tuple] = Schema { type TupleRepr = T }

    /**
      * Takes a type of [[Tuples.Tuple]], convert to a [[Schema]]
      *
      * e.g.
      *   - [A *: B *: _0] -> Schema[A ><: B ><: Empty]
      *   - [A *: _0] -> Schema[A ><: Empty]
      *   - Unit -> Schema[Empty]
      */
    def fromTuple[T <: Tuples.Tuple]: FromTuple[T] = {
      ???
    }

    type FromFlat[T] = Schema { type FlatRepr = T }

    /**
      * Takes a type of flat Scala tuple or Unit or value, convert to a [[Schema]]
      *
      * e.g.
      *   - [(A, B)] -> Schema[A ><: B ><: Empty]
      *   - [(A)] -> Schema[A ><: Empty]
      *   - [A] -> Schema[A ><: Empty]
      *   - Unit -> Schema[Empty]
      */
    implicit def fromFlat[T]: FromFlat[T] = {
      ???
    }
  }

//  trait FromTuple {}
//
//  trait FromTuple extends Hom.Poly {}
//  object FromTuple extends FromTuple {}
//
//  trait FromFlat extends Hom.Poly {}
//  object FromFlat extends FromFlat {}
//
//  /**
//    * similar to [[FromFlat]], but has a fallback case:
//    *
//    * that can convert any type A into `A ><: Empty`
//    */
//  trait FromFlatOrValue extends Hom.Poly {}
//  object FromFlatOrValue extends FromFlat {}
}

object SchemaMixin {}
