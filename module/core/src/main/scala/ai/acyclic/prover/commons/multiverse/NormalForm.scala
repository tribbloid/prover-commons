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

  implicit class Final(val value: Any) extends NormalForm {}

  def apply(value: Any): Final = Final(value)
}
