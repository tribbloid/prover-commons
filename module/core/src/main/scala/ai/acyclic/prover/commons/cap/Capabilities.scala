package ai.acyclic.prover.commons.cap

import scala.language.implicitConversions

object Capabilities {}

/**
  * capability tracking enabler without linear/affine type system, all subclasses of HasCap[_] can be freely cast into
  * HasCap[C] for any C in runtime.
  *
  * One of the rare cases where JVM type erasure could be useful.
  */
trait Capabilities extends CapabilityGroup {

  import Capability._

  trait Capability

  implicit def asAnnotator[C <: Capability](self: C): Annotator[C] = Annotator()
}
