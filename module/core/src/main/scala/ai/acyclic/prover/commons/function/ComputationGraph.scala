package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.graph.UnapplyInspection
import ai.acyclic.prover.commons.graph.viz.{Hierarchy, LinkedHierarchy}
import ai.acyclic.prover.commons.multiverse.{CanUnapply, Projection, UnappliedForm}
import ai.acyclic.prover.commons.debug.SrcDefinition

trait ComputationGraph extends Projection.Equals.ByConstruction {
  import ComputationGraph.*

  {
    definedAt // eager init
  }

  protected def _definedAt: SrcDefinition = SrcDefinition.Unknown(constructionID)
  final lazy val definedAt = _definedAt

  @transient object explain {

    private val node = Inspection.inspect(ComputationGraph.this)

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

object ComputationGraph {

  object Inspection extends UnapplyInspection {

    override lazy val primary: CanUnapply[Any] = {

      val proto = CanUnapply.Native.AndThen { ff =>
        val newPairs = ff.kvPairs.filter {
          case (_, v) =>
            v.isInstanceOf[ComputationGraph]
        }
        UnappliedForm.Pairs(newPairs, ff.prefix)
      }

      proto.ForAny
    }

    override lazy val inlined: CanUnapply[Any] = {

      object proto extends CanUnapply[ComputationGraph] {

        override def unapply(v: ComputationGraph): Option[UnappliedForm] = {
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
