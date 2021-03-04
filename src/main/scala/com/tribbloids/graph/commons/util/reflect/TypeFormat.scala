package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.ScalaReflection.{TypeTag, WeakTypeTag}
import com.tribbloids.graph.commons.util.TreeFormat
import com.tribbloids.graph.commons.util.viz.VizType

case class TypeFormat(
    showArgTree: Boolean = true,
    hidePackages: Boolean = false,
    hideAlias: Boolean = false,
    treeFormat: TreeFormat = TreeFormat.Indent2
) {}

object TypeFormat {

  object Default extends TypeFormat()
}
