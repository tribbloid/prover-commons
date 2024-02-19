package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.viz.format.FormattedType

case class TypeIROutput(
    text: String,
    annotations: Seq[FormattedType] = Nil,
    derivedFrom: Seq[FormattedType] = Nil
) {

  {
    // sanity check

    annotations.foreach { v =>
      val isSane = text.contains(v.text)
      require(
        isSane,
        s"""Annotation
           |$v
           |cannot be found in text
           |  '$text'""".stripMargin
      )
    }
  }

  def <:%(annotations: Seq[FormattedType]): TypeIROutput = this.copy(annotations = annotations)

  def <:~(derivedFrom: Seq[FormattedType]): TypeIROutput = this.copy(derivedFrom = derivedFrom)

  def <:^(derivedFrom: Seq[FormattedType]): TypeIROutput = {

    val transitiveAnnotations = derivedFrom.flatMap { v =>
      v.annotations
    }

    this.copy(annotations = transitiveAnnotations, derivedFrom = derivedFrom)
  }
}

object TypeIROutput {}
