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

    implicit def unitCase: Empty ~> Unit = new FlatSchema {
      override type Repr = Empty
      override type FlatRepr = Unit

      override def toRuntimeList(v: Inductive): List[Any] = scala.Nil
      override def fromRuntimeList(l: List[Any]): Inductive = Empty

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
      override def fromRuntimeList(l: List[Any]): Inductive = {
        cons(l.head.asInstanceOf[H], Empty)
      }

      override def forward(v: Inductive): H = {
        val (h, _) = deCons[H, Empty](v.asInstanceOf[Repr])
        h
      }
      override def reverse(v: H): Inductive = {
        cons(v, Empty)
      }
    }

    implicit def tuple2Case[T1 <: VBound, T2 <: VBound]: (T1 ><: T2 ><: Empty) ~> (T1, T2) = new FlatSchema {
      override type Repr = T1 ><: T2 ><: Empty
      override type FlatRepr = (T1, T2)

      override def toRuntimeList(v: Inductive): List[Any] = {
        val (t1, tail1) = deCons[T1, T2 ><: Empty](v.asInstanceOf[Repr])
        val (t2, _) = deCons[T2, Empty](tail1)
        List(t1, t2)
      }

      override def fromRuntimeList(l: List[Any]): Inductive = {
        cons(l(0).asInstanceOf[T1], cons(l(1).asInstanceOf[T2], Empty))
      }

      override def forward(v: Inductive): (T1, T2) = {
        val (t1, tail1) = deCons[T1, T2 ><: Empty](v.asInstanceOf[Repr])
        val (t2, _) = deCons[T2, Empty](tail1)
        (t1, t2)
      }

      override def reverse(v: (T1, T2)): Inductive = {
        cons(v._1, cons(v._2, Empty))
      }
    }

    implicit def tuple3Case[T1 <: VBound, T2 <: VBound, T3 <: VBound]: (T1 ><: T2 ><: T3 ><: Empty) ~> (T1, T2, T3) =
      new FlatSchema {
        override type Repr = T1 ><: T2 ><: T3 ><: Empty
        override type FlatRepr = (T1, T2, T3)

        override def toRuntimeList(v: Inductive): List[Any] = {
          val (t1, tail1) = deCons[T1, T2 ><: T3 ><: Empty](v.asInstanceOf[Repr])
          val (t2, tail2) = deCons[T2, T3 ><: Empty](tail1)
          val (t3, _) = deCons[T3, Empty](tail2)
          List(t1, t2, t3)
        }

        override def fromRuntimeList(l: List[Any]): Inductive = {
          cons(l(0).asInstanceOf[T1], cons(l(1).asInstanceOf[T2], cons(l(2).asInstanceOf[T3], Empty)))
        }

        override def forward(v: Inductive): (T1, T2, T3) = {
          val (t1, tail1) = deCons[T1, T2 ><: T3 ><: Empty](v.asInstanceOf[Repr])
          val (t2, tail2) = deCons[T2, T3 ><: Empty](tail1)
          val (t3, _) = deCons[T3, Empty](tail2)
          (t1, t2, t3)
        }

        override def reverse(v: (T1, T2, T3)): Inductive = {
          cons(v._1, cons(v._2, cons(v._3, Empty)))
        }
      }

    implicit def tuple4Case[
        T1 <: VBound,
        T2 <: VBound,
        T3 <: VBound,
        T4 <: VBound
    ]: (T1 ><: T2 ><: T3 ><: T4 ><: Empty) ~> (T1, T2, T3, T4) = new FlatSchema {
      override type Repr = T1 ><: T2 ><: T3 ><: T4 ><: Empty
      override type FlatRepr = (T1, T2, T3, T4)

      override def toRuntimeList(v: Inductive): List[Any] = {
        val (t1, tail1) = deCons[T1, T2 ><: T3 ><: T4 ><: Empty](v.asInstanceOf[Repr])
        val (t2, tail2) = deCons[T2, T3 ><: T4 ><: Empty](tail1)
        val (t3, tail3) = deCons[T3, T4 ><: Empty](tail2)
        val (t4, _) = deCons[T4, Empty](tail3)
        List(t1, t2, t3, t4)
      }

      override def fromRuntimeList(l: List[Any]): Inductive = {
        cons(
          l(0).asInstanceOf[T1],
          cons(l(1).asInstanceOf[T2], cons(l(2).asInstanceOf[T3], cons(l(3).asInstanceOf[T4], Empty)))
        )
      }

      override def forward(v: Inductive): (T1, T2, T3, T4) = {
        val (t1, tail1) = deCons[T1, T2 ><: T3 ><: T4 ><: Empty](v.asInstanceOf[Repr])
        val (t2, tail2) = deCons[T2, T3 ><: T4 ><: Empty](tail1)
        val (t3, tail3) = deCons[T3, T4 ><: Empty](tail2)
        val (t4, _) = deCons[T4, Empty](tail3)
        (t1, t2, t3, t4)
      }

      override def reverse(v: (T1, T2, T3, T4)): Inductive = {
        cons(v._1, cons(v._2, cons(v._3, cons(v._4, Empty))))
      }
    }
  }
}

object SchemaMixin {}
