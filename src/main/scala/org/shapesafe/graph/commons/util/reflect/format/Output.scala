package org.shapesafe.graph.commons.util.reflect.format

import scala.language.implicitConversions

case class Output(
    text: String,
    parts: Seq[FormattedType] = Nil,
    simplified: Option[FormattedType] = None
) {}

object Output {}
