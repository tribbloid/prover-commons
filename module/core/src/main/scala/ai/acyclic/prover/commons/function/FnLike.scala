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

    val suffix = this match {

      case v: Transparent => s" <~ (${v.references.mkString(" , ")})"
      case _              => ""
    }
    body + suffix
  }
}

/**
  * can mixin [[Capabilities.Cap]], but so far, the only [[Cap]] is for refining candidates of polymorphic cases
  */
object FnLike extends Capabilities {

  trait Transparent extends FnLike {

    def references: Seq[FnLike]
  }

  trait Transparent1 extends Transparent {

    def reference: FnLike

    final lazy val references = Seq(reference)
  }
}
