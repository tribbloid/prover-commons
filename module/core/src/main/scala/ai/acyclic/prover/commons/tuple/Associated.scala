package ai.acyclic.prover.commons.tuple

import scala.language.implicitConversions

/**
  * Base trait for associated tuple systems. Defines common types and operations for both left and right nested tuple
  * systems.
  */
trait Associated {

  type VBound

  type Prod

  protected val _1: Prod

  /**
    * Identity element of the product (MATLAB terminology)
    */
  final def Eye: Eye = _1
  type Eye = _1.type

  type Nil = Eye
  val Nil: Nil = Eye
}
