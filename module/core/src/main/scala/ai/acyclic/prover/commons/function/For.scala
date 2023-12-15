package ai.acyclic.prover.commons.function

trait For[T] {

  import Symbolic._

//  trait >[+R] extends (T :=> R) TODO this is useless due to lack of full dependent type

  def :=>[R](fn: T => R): T :=> R = ???
}

object For {

  def apply[T]: For[T] = new For[T] {}
}
