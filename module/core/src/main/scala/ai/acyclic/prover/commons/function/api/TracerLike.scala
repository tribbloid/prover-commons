package ai.acyclic.prover.commons.function.api

trait TracerLike extends Explainable {

  type In
  type Out

  /**
    * the only Single Abstract Method interface
    * @param arg
    *   input (can be product type)
    * @return
    *   output (can be curried function)
    */
  def apply(arg: In): Out

}

object TracerLike {}
