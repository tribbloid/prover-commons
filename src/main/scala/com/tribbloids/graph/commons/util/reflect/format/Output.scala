package com.tribbloids.graph.commons.util.reflect.format

import scala.language.implicitConversions

case class Output(
    text: String,
    parts: Seq[Formatting] = Nil
) {}

object Output {}
