package com.tribbloids.graph.commons.util.reflect.format

import scala.language.implicitConversions

case class Output(
    text: String,
    children: Seq[Formatting] = Nil
) {}

object Output {}
