package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.function.PreDef
import shapeless.HNil

trait TupleSystem {

  type VBound

  type Tuple

  type Eye <: Tuple
  val Eye: Eye

  trait HListIntake extends PreDef.Poly {

    final val outer = TupleSystem.this

    implicit val toEye: HNil =>> Eye = {
      at[HNil].defining { _ =>
        Eye
      }
    }
  }

  object HListIntake {}
}

object TupleSystem {}
