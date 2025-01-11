package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.graph.UnapplyInspection
import ai.acyclic.prover.commons.graph.viz.{Hierarchy, LinkedHierarchy}
import ai.acyclic.prover.commons.multiverse.{CanUnapply, UnappliedForm, Projection}
import ai.acyclic.prover.commons.util.SrcDefinition

trait Traceable extends Projection.Equals.ByConstruction {
  import Traceable.*

  {
    definedAt // eager init
  }

  protected def _definedAt: SrcDefinition = SrcDefinition.Unknown(constructionID)
  final lazy val definedAt = _definedAt

  @transient object explain {

    private val node = Inspection.inspect(Traceable.this)

    def nodeText: String = {
      node.nodeText
    }

    def text_hierarchy(): String = {

      val viz = Hierarchy.Default.showNode(node)
      viz.toString
    }

    def text_linkedHierarchy(): String = {
      val viz = LinkedHierarchy.Default.showNode(node)
      viz.toString
    }
  }
}

object Traceable {

  object Inspection extends UnapplyInspection {

    override lazy val primary: CanUnapply[Any] = {

      val proto = CanUnapply.Native.AndThen { ff =>
        val newPairs = ff.kvPairs.filter {
          case (_, v) =>
            v.isInstanceOf[Traceable]
        }
        UnappliedForm.Pairs(newPairs, ff.prefix)
      }

      proto.ForAny
    }

    override lazy val inlined: CanUnapply[Any] = {

      object proto extends CanUnapply[Traceable] {

        override def unapply(v: Traceable): Option[UnappliedForm] = {
          v match {
            case vv: Product => CanUnapply.Native.unapply(vv)
            case _ =>
              Some(
                UnappliedForm.Tuple(Vector(v.definedAt), v.getClass.getSimpleName)
              )
          }
        }
      }

      proto.ForAny / primary
    }

  }

}
