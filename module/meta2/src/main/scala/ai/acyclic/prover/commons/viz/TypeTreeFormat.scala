package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy.RefBindingLike
import ai.acyclic.prover.commons.graph.viz.{Flow, Hierarchy, LinkedHierarchy}
import ai.acyclic.prover.commons.viz.format.TypeFormat

import scala.language.implicitConversions

case class TypeTreeFormat(
    typeFormat: TypeFormat,
    backbone: Hierarchy = Hierarchy.Default,
    showArgs: TypeTreeFormat.ShowArgs = TypeTreeFormat.ShowArgs.default
) {

  lazy val _showArgs: Boolean = showArgs != TypeTreeFormat.NoArg

  lazy val _expandArgsBeforeSubtypes: Boolean = showArgs == TypeTreeFormat.ArgsBeforeSuperTypes

  val _FlowFormat: Flow.Default.type = Flow.Default

  // TODO : this has to be removed! otherwise TypeVisualization can only use 1 format!
  object _LinkedHierarchyFormat extends LinkedHierarchy.Default(backbone) {

    override def scanLinks(tree: Local.Diverging.Tree[RefBindingLike]): Unit = {

      lazy val scanArgs: Unit = {

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

        scanArgs
        super.scanLinks(tree)
      } else {

        super.scanLinks(tree)
        scanArgs
      }
    }
  }

}

object TypeTreeFormat {

  sealed trait ShowArgs

  object ShowArgs {

    def default: ShowArgs = SuperTypesBeforeArgs
  }

  object ArgsBeforeSuperTypes extends ShowArgs
  object SuperTypesBeforeArgs extends ShowArgs
  object NoArg extends ShowArgs

  object Default
      extends TypeTreeFormat(
        TypeFormat.Default
      )

  implicit def fromBase(base: TypeFormat): TypeTreeFormat = TypeTreeFormat(base)
}
