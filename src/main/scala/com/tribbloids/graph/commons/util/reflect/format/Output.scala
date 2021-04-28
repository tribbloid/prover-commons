package com.tribbloids.graph.commons.util.reflect.format

import scala.language.implicitConversions

case class Output(
    text: String,
    causes: Seq[Formatting] = Nil
)

object Output {}
