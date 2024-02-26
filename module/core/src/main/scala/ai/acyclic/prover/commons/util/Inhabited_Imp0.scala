package ai.acyclic.prover.commons.util

trait Inhabited_Imp0 {
  // Stop AnyRefs from clashing with AnyVals
  implicit def defaultNull[A <: AnyRef]: Inhabited[A] = new Inhabited[A](null.asInstanceOf[A])
}
