package org.shapesafe.graph.commons.util.reflect.format

import scala.language.implicitConversions

case class Output(
    text: String,
    parts: Seq[Formatting] = Nil,
    equivalent: Option[Formatting] = None
) {}

object Output {}
