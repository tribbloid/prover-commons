package org.shapesafe.graph.commons.util.reflect.format

case class IROutput(
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
           |  '${v.text}'
           |cannot be found in text
           |  '$text'""".stripMargin
      )
    }
  }

  def <:%(annotations: Seq[FormattedType]): IROutput = this.copy(annotations = annotations)

  def <:~(derivedFrom: Seq[FormattedType]): IROutput = this.copy(derivedFrom = derivedFrom)

  def <:^(derivedFrom: Seq[FormattedType]): IROutput = {

    val transitiveAnnotations = derivedFrom.flatMap { v =>
      v.annotations
    }.distinct

    this.copy(annotations = transitiveAnnotations, derivedFrom = derivedFrom)
  }
}

object IROutput {}
