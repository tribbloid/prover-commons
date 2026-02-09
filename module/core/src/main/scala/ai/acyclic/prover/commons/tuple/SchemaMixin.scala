package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom.Poly
import shapeless.ops.hlist.Tupler

import scala.language.implicitConversions
import shapeless.{::, HList, HNil}

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
  object ToFlatRepr extends Poly {

    implicit def default[I <: self.Inductive, L <: HList, O](
        implicit
        toTuple: self.ToTuple.|-[I, L],
        hlistToFlat: Tupler.Aux[L, O]
    ): I |- O = at[I] { i =>
      hlistToFlat(toTuple(i))
    }
  }

  /**
    * The inverse of [[ToFlatRepr]]
    */
  object FromFlatRepr extends Poly {}

}

object SchemaMixin {}
