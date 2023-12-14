package ai.acyclic.prover.commons.compose

import ai.acyclic.prover.commons.util.DefinedAtMixin

import scala.runtime.ScalaRunTime

trait FnLike extends DefinedAtMixin {

  override def toString: String = {
    this match {
      case p: Product =>
        ScalaRunTime._toString(p) // TODO: use tree string
      case _ =>
        "(defined at: " + definedAt + ")"
    }
  }
}
object FnLike {

  trait PureTag {
    self: FnLike =>
  }
}
