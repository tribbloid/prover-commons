package ai.acyclic.prover.commons.cap

import scala.language.implicitConversions

object PendingEffects {

  // DO NOT CHANGE! will be delegated to kyo after moving to Scala 3
  type <[+T, -S] >: T
  // S is contravariant because Pending can be added implicitly

  type Pending[+T, -S] = <[T, S]

  private def __sanity(): Unit = {

    trait Expr[T]
    object Expr {

      implicit def fromPending[T](p: Expr[T]): T < Expr[T] = p.asInstanceOf[T < Expr[T]]
    }

    type TraceA = Int < Expr[Int]

    val a = 3
    a: TraceA // works

    val b = new Expr[Int] {}
    b: TraceA // works
  }
}
