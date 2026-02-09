package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom.Poly

import scala.language.implicitConversions
import shapeless.{::, <:!<, =:!=, Generic, HList, HNil}

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
        hlistToFlat: SchemaMixin.HListToFlat.Aux[L, O]
    ): I |- O = at[I] { i =>
      hlistToFlat(toTuple(i))
    }
  }

  /**
    * The inverse of [[ToFlatRepr]]
    */
  object FromFlatRepr extends Poly {}

}

object SchemaMixin {

  trait HListToFlat[L <: HList] {
    type Out
    def apply(l: L): Out
  }

  object HListToFlat {
    type Aux[L <: HList, O] = HListToFlat[L] { type Out = O }

    // Fix for HNil.type invariance in Tupler not needed with explicit cases

    implicit def hnilCase[L <: HList](
        implicit
        ev: L <:< HNil
    ): Aux[L, Unit] = new HListToFlat[L] {
      type Out = Unit
      def apply(l: L): Unit = ()
    }

    implicit def singleCase[H, T <: HList](
        implicit
        ev: T <:< HNil
    ): Aux[H :: T, H] = new HListToFlat[H :: T] {
      type Out = H
      def apply(l: H :: T): H = l.head
    }

    implicit def tuple2Case[H1, H2, T <: HList](
        implicit
        ev: T <:< HNil
    ): Aux[H1 :: H2 :: T, (H1, H2)] = new HListToFlat[H1 :: H2 :: T] {
      type Out = (H1, H2)
      def apply(l: H1 :: H2 :: T): (H1, H2) = (l.head, l.tail.head)
    }

    implicit def tuple3Case[H1, H2, H3, T <: HList](
        implicit
        ev: T <:< HNil
    ): Aux[H1 :: H2 :: H3 :: T, (H1, H2, H3)] = new HListToFlat[H1 :: H2 :: H3 :: T] {
      type Out = (H1, H2, H3)
      def apply(l: H1 :: H2 :: H3 :: T): (H1, H2, H3) = (l.head, l.tail.head, l.tail.tail.head)
    }

    // fallback for larger tuples could use Tupler if we can fix the recursion, but for now explicit is safer.
  }
}
