package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy.RefBindingLike
import ai.acyclic.prover.commons.graph.viz.{Hierarchy, LinkedHierarchy}
import ai.acyclic.prover.commons.meta.format.TypeFormat

import scala.language.implicitConversions

case class TypeHierarchy(
    typeFormat: TypeFormat,
    backbone: Hierarchy = Hierarchy.default,
    showArgs: TypeHierarchy.ShowArgs = TypeHierarchy.ShowArgs.default
) {

  lazy val _showArgs: Boolean = showArgs != TypeHierarchy.NoArg

  lazy val _expandArgsBeforeSubtypes: Boolean = showArgs == TypeHierarchy.ArgsBeforeSuperTypes

  object GraphFormat extends LinkedHierarchy.Default(backbone) {

//    final override def sameRefBy(node: Any): Option[Any] = {
//      node match {
//        case v: TypeOfMixin#VNode =>
//          v.sameRefBy
//        case _ =>
//          None
//      }
//    }

    override def dryRun(tree: Local.Tree[_ <: RefBindingLike]): Unit = {

      def recursiveDryRun(): Unit = {

        tree
          .Traverse(
            down = { v: Local.Tree.Node[_ <: RefBindingLike] =>
              v.value.original match {
                case vNode: TypeOfMixin.VNodeLike =>
                  vNode.argDryRun()
                case _ => // do nothing
              }
            }
          )
          .DepthFirst
          .resolve
      }

      if (_expandArgsBeforeSubtypes) {

        recursiveDryRun()
        super.dryRun(tree)
      } else {

        super.dryRun(tree)
        recursiveDryRun()
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
