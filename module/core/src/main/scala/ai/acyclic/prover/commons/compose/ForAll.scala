package ai.acyclic.prover.commons.compose

trait ForAll[-T] {

  import Fn._

  trait >[+R] extends (T :=> R)
}

object ForAll {

  def apply[T]: ForAll[T] = new ForAll[T] {}
}
