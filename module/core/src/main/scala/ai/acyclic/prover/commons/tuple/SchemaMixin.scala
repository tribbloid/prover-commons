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
  trait Schema {

    type Repr <: Inductive

    def toRuntimeList(v: Inductive): List[Any]
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

    implicit def unitCase: Empty ~> Unit = new FlatSchema {
      override type Repr = Empty
      override type FlatRepr = Unit

      override def toRuntimeList(v: Inductive): List[Any] = scala.Nil

      override def forward(v: Inductive): Unit = ()
      override def reverse(v: Unit): Inductive = Empty
    }

    implicit def valueCase[H <: VBound]: (H ><: Empty) ~> H = new FlatSchema {
      override type Repr = H ><: Empty
      override type FlatRepr = H

      override def toRuntimeList(v: Inductive): List[Any] = {
        val (h, _) = deCons[H, Empty](v.asInstanceOf[Repr])
        List(h)
      }

      override def forward(v: Inductive): H = {
        val (h, _) = deCons[H, Empty](v.asInstanceOf[Repr])
        h
      }
      override def reverse(v: H): Inductive = {
        cons(v, Empty)
      }
    }

    implicit def genericTupleCase[
        I <: Inductive,
        H <: shapeless.HList,
        T <: Product
    ](
        implicit
        toHList: ToTuple.Impl[I, H],
        fromHList: FromTuple.Impl[H, I],
        tupler: shapeless.ops.hlist.Tupler.Aux[H, T],
        gen: shapeless.Generic.Aux[T, H]
//        hListRuntime: HListRuntime[H]
    ): I ~> T = new FlatSchema {
      override type Repr = I
      override type FlatRepr = T

      override def toRuntimeList(v: Inductive): List[Any] = {
        val h = toHList(v.asInstanceOf[I])
        h.runtimeList
      }

      override def forward(v: Inductive): T = {
        val h = toHList(v.asInstanceOf[I])
        tupler(h)
      }

      override def reverse(v: T): Inductive = {
        val h = gen.to(v)
        fromHList(h)
      }
    }

    implicit def identityToTuple[H <: shapeless.HList]: ToTuple.Impl[H, H] =
      ToTuple.at[H](h => h)

    implicit def identityFromTuple[H <: shapeless.HList]: FromTuple.Impl[H, H] =
      FromTuple.at[H](h => h)
  }
}

object SchemaMixin {}
