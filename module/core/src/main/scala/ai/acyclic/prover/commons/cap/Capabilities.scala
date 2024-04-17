package ai.acyclic.prover.commons.cap

import ai.acyclic.prover.commons.function.Hom

import scala.language.implicitConversions

object Capabilities {

  trait _Can[+T]

  // CC is covariant because Capabilities can be revoked implicitly

//  object ^: {
//
//    implicit def revokeAll[T, C](self: T ^: C): T = self.asInstanceOf[T]
//  }
//
//  implicit def revokeAll[T, C](self: T ^: C): T = self.asInstanceOf[T]

}

/**
  * capability tracking enabler without linear/affine type system, all subclasses of HasCap[_] can be freely cast into
  * HasCap[C] for any C in runtime.
  *
  * One of the rare cases where JVM type erasure could be useful.
  */
trait Capabilities {

  import Capabilities._

  type ^:[+T, +C <: Capability] <: T with _Can[C] // following the convention of Scala 3.4.0 with capture checking

//  implicit def withRootCap[T](v: T): T ^: Capability = v.asInstanceOf[T ^: Capability]

  trait Capability {}

  sealed trait Add[C <: Capability] {

    def apply[V](v: V): V ^: C = v.asInstanceOf[V ^: C]
  }

  sealed trait Revoke[C <: Capability] {

    def apply[V](v: V ^: C): V = v.asInstanceOf[V]
  }

  case class Annotator[C <: Capability]() {

    object add extends Add[C]

    // TODO: sometimes left associated function won't work (generic collapse to Nothing), need to file a bug report for it
    def ^: : add.type = add

    object revoke extends Revoke[C]

    def --^: : revoke.type = revoke
  }

  implicit def asAnnotator[C <: Capability](self: C): Annotator[C] = Annotator()

  trait revokeAll_Lvl0 extends Hom.Poly {

    implicit def last[T, C <: Capability]: (T ^: C) =>> T = at[T ^: C] { v =>
      v.asInstanceOf[T]
    }
  }

  object revokeAll extends revokeAll_Lvl0 {

    implicit def chain[T, C <: Capability](
        implicit
        lemma: At[T]
    ): (T ^: C) =>> lemma.Out = at[T ^: C] { v =>
      lemma(v.asInstanceOf[T])
    }
  }

  trait CanEnable {

//    def enable[CC <: Capability] = {
//
//      val mixin = new Annotator[CC] {}
//      CanEnable.this ^: mixin
//    }
  }

}
