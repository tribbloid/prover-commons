package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom
import shapeless.HNil

trait TupleSystem {

  type VBound

  type Tuple

  type Eye <: Tuple
  val Eye: Eye

  trait HListIntake extends Hom.Poly {

    final val outer = TupleSystem.this

    implicit val toEye: HNil |- Eye = {
      at[HNil] { _ =>
        Eye
      }
    }
  }

  object HListIntake {}
}

object TupleSystem {}
