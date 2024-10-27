package ai.acyclic.prover.commons.cap

import scala.language.implicitConversions

trait Capability

object Capability extends CapabilityGroup {

  trait Universe extends CapabilityGroup {}
}
