package ai.acyclic.prover.commons.util

trait PhantomAuto extends PhantomAuto.Case

object PhantomAuto extends StaticGroup {

  /**
    * return an ad-hoc instance of [[Case]] with compatible class header
    *
    * assuming that [[Case]] contains no data
    *
    * implementation should be short and only use language features that are available in both Scala 2 and Scala 3
    */
  implicit def onlyCase[T <: Case]: T = {
    ???
  }
}
