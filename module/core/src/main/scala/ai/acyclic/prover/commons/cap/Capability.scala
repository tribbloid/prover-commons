package ai.acyclic.prover.commons.cap

object Capability extends CapabilityGroup {

  final override type Capability = Any

  trait _Can[+C]

  type <>[+T, +C] <: T with _Can[C]

  sealed trait Add[C] {

    def apply[V](v: V): V <> C = v.asInstanceOf[V <> C]
  }

  sealed trait Revoke[C] {

    def apply[V](v: V <> C): V = v.asInstanceOf[V]
  }

  case class Annotator[C]() {

    object add extends Add[C]

    // TODO: sometimes left associated function won't work (generic collapse to Nothing), need to file a bug report for it
    def <>: : add.type = add

    object revoke extends Revoke[C]

  }
}
