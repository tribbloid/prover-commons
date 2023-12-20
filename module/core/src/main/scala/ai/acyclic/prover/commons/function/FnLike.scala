package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.util.{Capabilities, DefinedAtMixin}

import scala.runtime.ScalaRunTime

trait FnLike extends DefinedAtMixin with FnLike.NoCap {
  import FnLike._

  {
    toString // force lazy val
  }

  override lazy val toString: String = {
    val body = this match {
      case v: Product =>
        ScalaRunTime._toString(v) // TODO: use tree string
      case _ =>
        s"<defined at: $definedAt>"
    }

    val prefix = this match {

      case v: Derived => v.derivedFrom.toString + "."
      case _          => ""
    }
    prefix + body
  }
}

/**
  * can mixin [[Capabilities.Cap]], but so far, the only [[Cap]] is for refining candidates of polymorphic cases
  */
object FnLike extends Capabilities {

  trait Derived extends FnLike {

    def derivedFrom: FnLike
  }
}
