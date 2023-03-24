package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.graph.local.Tree
import ai.acyclic.prover.commons.graph.plan.local.GraphUnary
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy._RefNode
import ai.acyclic.prover.commons.graph.viz.{Hierarchy, LinkedHierarchy}
import ai.acyclic.prover.commons.reflect.format.TypeFormat

import scala.language.implicitConversions

case class TypeHierarchy(
    typeFormat: TypeFormat,
    backbone: Hierarchy = Hierarchy.default,
    showArgs: TypeHierarchy.ShowArgs = TypeHierarchy.ShowArgs.default
) {

  lazy val _showArgs: Boolean = showArgs != TypeHierarchy.NoArg

  lazy val _expandArgsBeforeSubtypes: Boolean = showArgs == TypeHierarchy.ArgsBeforeSuperTypes

  object GraphFormat extends LinkedHierarchy.Default(backbone) {

    final override def sameRefBy(node: Any): Option[Any] = {
      node match {
        case v: TypeOfMixin#VNode =>
          v.sameRefBy
        case _ =>
          None
      }
    }

    override def dryRun[N <: _RefNode](g: Tree[N]): Unit = {

      def argDryRun(): Unit = {

        GraphUnary
          .make(g)
          .Traverse(
            down = { v: N =>
              v.node match {
                case vNode: TypeOfMixin#VNode =>
                  vNode.argDryRun()
                case _ => // do nothing
              }
            }
          )
          .DepthFirst
          .exe
      }

      if (_expandArgsBeforeSubtypes) {

        argDryRun()
        super.dryRun(g)
      } else {

        super.dryRun(g)
        argDryRun()
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
