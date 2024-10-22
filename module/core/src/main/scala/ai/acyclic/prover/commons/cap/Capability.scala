package ai.acyclic.prover.commons.cap

import scala.language.implicitConversions

object Capability extends CapabilityGroup {

  trait _Can[+C]

  type <>[+T, +C] <: T with _Can[C]

  final override type Capability = Any

  sealed trait AddFunction[C] {

    def apply[V](v: V): V <> C = v.asInstanceOf[V <> C]
  }

  sealed trait Revoke[C] {

    def apply[V](v: V <> C): V = v.asInstanceOf[V]
  }

  trait Universe extends CapabilityGroup {

    trait Capability

//    implicit def asAnnotator[C <: Capability](self: C): Annotator[C] = Annotator()

    implicit class Annotator[C](self: C) {

      object add extends AddFunction[C]

      // TODO: sometimes left associated function won't work (generic collapse to Nothing), need to file a bug report for it
      def <>: : add.type = add

      object revoke extends Revoke[C]
    }
  }

}
