package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy.RefBindingLike
import ai.acyclic.prover.commons.graph.viz.{Hierarchy, LinkedHierarchy}
import ai.acyclic.prover.commons.viz.format.TypeFormat

import scala.language.implicitConversions

case class TypeHierarchy(
    typeFormat: TypeFormat,
    backbone: Hierarchy = Hierarchy.Default,
    showArgs: TypeHierarchy.ShowArgs = TypeHierarchy.ShowArgs.default
) {

  lazy val _showArgs: Boolean = showArgs != TypeHierarchy.NoArg

  lazy val _expandArgsBeforeSubtypes: Boolean = showArgs == TypeHierarchy.ArgsBeforeSuperTypes

  // TODO : this has to be removed! otherwise TypeVisualization can only use 1 format!
  object DelegateFormat extends LinkedHierarchy.Default(backbone) {

    override def scanReferences(tree: Local.Diverging.Tree[RefBindingLike]): Unit = {

      lazy val scanArgsOnce: Unit = {

        val _tree = tree.ops_anyGraph

        _tree
          .Traverse(
            down = { (v: Local.Diverging.Tree.Node[RefBindingLike]) =>
              v.value.original match {
                case vNode: TypeOfMixin.VNodeLike =>
                  vNode.scanArgsOnce
                case _ => // do nothing
              }
            }
          )
          .DepthFirst
      }

      if (_expandArgsBeforeSubtypes) {

        scanArgsOnce
        super.scanReferences(tree)
      } else {

        super.scanReferences(tree)
        scanArgsOnce
      }
    }
  }

}

object TypeHierarchy {

  sealed trait ShowArgs

  object ShowArgs {

    def default: ShowArgs = SuperTypesBeforeArgs
  }

  object ArgsBeforeSuperTypes extends ShowArgs
  object SuperTypesBeforeArgs extends ShowArgs
  object NoArg extends ShowArgs

  object Default
      extends TypeHierarchy(
        TypeFormat.Default
      )

  implicit def fromBase(base: TypeFormat): TypeHierarchy = TypeHierarchy(base)
}
