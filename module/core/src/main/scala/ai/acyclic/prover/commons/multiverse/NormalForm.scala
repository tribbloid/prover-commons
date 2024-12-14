package ai.acyclic.prover.commons.multiverse

import ai.acyclic.prover.commons.multiverse.NormalForm.Like

trait NormalForm extends Like {
  type T = Any
}

object NormalForm {

  trait Like {

    type T // defaults to Any

    def value: T

    // TODO: this has to be enabled to indicate weakly-normalising
//    def isFinal: Boolean
  }

  case class ^(value: Any, isFinal: Boolean = true) extends NormalForm {}

  def apply(value: Any, isFinal: Boolean = true): ^ = ^(value, isFinal)
}
