package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.graph.local.DestructuringInspection.Destructured
import ai.acyclic.prover.commons.graph.local.{DestructuringInspection, ProductInspection}
import ai.acyclic.prover.commons.graph.viz.{Hierarchy, LinkedHierarchy}
import ai.acyclic.prover.commons.same.View
import ai.acyclic.prover.commons.util.SrcDefinition

trait Traceable extends View.Equals.ByConstruction {
  import Traceable.*

  {
    definedAt // eager init
  }

  protected def _definedAt: SrcDefinition = SrcDefinition.Unknown(constructionID)
  final lazy val definedAt = _definedAt

  @transient object explain {

    private val node = Inspection.node(Traceable.this)

    def nodeText: String = {
      node.nodeText
    }

    def text_hierarchy: String = {

      val viz = Hierarchy.Default.visualiseNode(node)
      viz.toString
    }

    def text_linkedHierarchy: String = {
      val viz = LinkedHierarchy.Default.visualiseNode(node)
      viz.toString
    }
  }

  override def toString = explain.nodeText
}

object Traceable {

  object Inspection extends DestructuringInspection[Traceable, Traceable] {

    override def unapplyAll(value: Traceable): Destructured[Traceable] = {

      val raw: Destructured[Traceable] = value match {
        case vv: Traceable with Product =>
          ProductInspection.unapplyProduct(vv, Nil)
        case vv: Product =>
          Destructured(vv.productPrefix, Nil, Seq(value.definedAt))
        case _ =>
          // not a product, both extensional equality & visualization have to rely on SrcDefinition
          Destructured(value.getClass.getSimpleName, Nil, Seq(value.definedAt))
      }

      raw
    }

    final override val node: Traceable => BuiltIn = { v =>
      BuiltIn(v)
    }
  }

//  trait Leaf extends Traceable with Product1[SrcPosition] {
//
//    def _1: SrcPosition = definedAt
//  }
//
//  object Leaf {
//
//    abstract class CompileTime(
//        implicit
//        _defineAt: SrcPosition
//    ) extends Leaf
//  }

}
