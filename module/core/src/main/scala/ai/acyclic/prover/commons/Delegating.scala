package ai.acyclic.prover.commons

import scala.language.implicitConversions

trait Delegating[T] {

  import Delegating._

  protected def delegate: T

  def unbox[R](
      implicit
      unbox: Unboxing.=>>[Delegating[T], R]
  ): R = unbox(this)
}

object Delegating {

  object Unboxing {
    // not using Poly to avoid overhead

    trait =>>[T, R] extends (T => R) {}

    implicit def unbox1[T]: =>>[Delegating[T], T] = { v =>
      v.delegate
    }

    implicit def unboxMore[T, R](
        implicit
        continuation: =>>[T, R]
    ): =>>[Delegating[T], R] = { v =>
      continuation(v.delegate)
    }
  }

  implicit def unboxImplicitly[T](v: Delegating[T]): T = v.unbox

  private def __santiy: Unit = {

    case class B1(delegate: String) extends Delegating[String]
    case class B2(delegate: B1) extends Delegating[B1]

    val b2 = B2(B1("hello"))

    b2.unbox.concat("b")
//    b2.concat("b") // oops, doesn't work
  }
}
