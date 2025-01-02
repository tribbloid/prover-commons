package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.Delegating.Unboxing.Conversion

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

  trait Unboxing_Imp0 {

    implicit def unboxMore[T, R](
        implicit
        continuation: Conversion[T, R]
    ): Conversion[Delegating[T], R] = { v =>
      continuation(v.unbox)
    }
  }

  object Unboxing extends Unboxing_Imp0 {
    // not using Poly to avoid overhead

    trait Conversion[-T, +R] extends (T => R) {} // TODO: has a canonical impl in Scala 3

    implicit def unbox1[T]: Conversion[Delegating[T], T] = { v =>
      v.unbox
    }

  }

  implicit def unboxImplicitly[T](v: Delegating[T]): T = v.as

  private def __santiy: Unit = {

    case class B1(unbox: String) extends Delegating[String]
    case class B2(unbox: B1) extends Delegating[B1]
    case class B3(unbox: B2) extends Delegating[B2]

    val b2 = B2(B1("hello"))
    val b3 = B3(b2)

    b2.as.concat("b")
    //    b2.concat("b") // oops, doesn't work, TODO: fix in Scala 3

    b3.as.as.concat("b")
//    b3.as.concat("b") // TODO: fix in Scala 3
  }
}
