package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.graph.viz.Hierarchy
import ai.acyclic.prover.commons.reflect.format.TypeFormat

import scala.language.implicitConversions

// TODO: should be TypeSemilatticeFormat
case class TypeVizFormat(
    base: TypeFormat,
    showArgs: Boolean = true,
    treeFormat: Hierarchy = Hierarchy.Indent2
) {}

object TypeVizFormat {

  object Default
      extends TypeVizFormat(
        TypeFormat.Default
      )

  implicit def fromBase(base: TypeFormat): TypeVizFormat = TypeVizFormat(base)
}
