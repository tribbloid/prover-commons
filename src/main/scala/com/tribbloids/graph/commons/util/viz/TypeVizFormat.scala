package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.TreeFormat
import com.tribbloids.graph.commons.util.reflect.TypeFormat

// TODO: this should be broken into type format and tree format
case class TypeVizFormat(
    base: TypeFormat,
    showArgTree: Boolean = true,
    treeFormat: TreeFormat = TreeFormat.Indent2
) {}
