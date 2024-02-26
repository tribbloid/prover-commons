package ai.acyclic.prover.commons.multiverse

import scala.language.implicitConversions

/**
  * implicit function allows [[R]] to act as an extension of [[V]]
  */
trait CanInspect[-V, R] {

//  lazy val bracket: (String, String) = "(" -> ")"

  type Result = R

  val inspect: V => R

  implicit def asNode(v: V): R = inspect(v)
  implicit def asNodes(vs: Seq[V]): Seq[R] = vs.map(inspect)
}
