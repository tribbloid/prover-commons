package ai.acyclic.prover.commons

import scala.language.implicitConversions

trait Delegating[+T] {

  import Delegating.*

  def unbox: T

  def as[R](
      implicit
      lemma: Unboxing.Conversion[Delegating[T], R]
  ): R = lemma(this)
}

object Delegating {

  object Unboxing {
    // not using Poly to avoid overhead

    trait Conversion[-T, +R] extends (T => R) {} // TODO: has a canonical impl in Scala 3

    implicit def unbox1[T]: Conversion[Delegating[T], T] = { v =>
      v.unbox
    }

    implicit def unboxMore[T, R](
        implicit
        continuation: Conversion[T, R]
    ): Conversion[Delegating[T], R] = { v =>
      continuation(v.unbox)
    }
  }

  implicit def unboxImplicitly[T](v: Delegating[T]): T = v.as

  private def __santiy: Unit = {

    case class B1(unbox: String) extends Delegating[String]
    case class B2(unbox: B1) extends Delegating[B1]

    val b2 = B2(B1("hello"))

    b2.as.concat("b")
//    b2.concat("b") // oops, doesn't work, TODO: fix in Scala 3
  }
}
