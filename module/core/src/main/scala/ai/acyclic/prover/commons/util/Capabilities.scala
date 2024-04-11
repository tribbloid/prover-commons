package ai.acyclic.prover.commons.util

import scala.language.implicitConversions

/**
  * capability tracking enabler without linear/affine type system, all subclasses of HasCap[_] can be freely cast into
  * HasCap[C] for any C in runtime.
  *
  * One of the rare cases where JVM type erasure could be useful.
  */
trait Capabilities {

  trait Capability {}

  object Capability {

    case class CapAsAnnotator[C <: Capability](self: C) extends Annotator[C]

    implicit def asAnnotator[C <: Capability](self: C): CapAsAnnotator[C] = CapAsAnnotator(self)
  }

  sealed trait _Can[+C <: Capability] {
    // DO NOT use outside this class, will trigger ClassCastException unless mixin

    //    def enable[CC <: Cap]: _Can.this.type ^^ CC = this.asInstanceOf[this.type ^^ CC]
  }

  type ^:[+T, +CC <: Capability] = T with _Can[CC] // following the convention of Scala 3.4.0 with capture checking

  trait Annotator[CC <: Capability] {

    // TODO: sometimes left associated function won't work (generic collapse to Nothing), need to file a bug report for it
    def ^:[V](v: V): V with v.type ^: CC = v.asInstanceOf[V with v.type ^: CC]
  }

  trait CanEnable extends _Can[Capability] {

    def enable[CC <: Capability] = {

      val mixin = new Annotator[CC] {}
      CanEnable.this ^: mixin
    }
  }

}
