package com.tribbloids.graph.commons.util.reflect

import com.tribbloids.graph.commons.util.TreeFormat

case class TypeFormat(
    showArgTree: Boolean = true,
    hidePackages: Boolean = false,
    hideAlias: Boolean = false,
    treeFormat: TreeFormat = TreeFormat.Indent2
) {}

object TypeFormat {

  object Default extends TypeFormat()
}
