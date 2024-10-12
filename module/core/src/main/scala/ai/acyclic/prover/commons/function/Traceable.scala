package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.debug.CallStackRef
import ai.acyclic.prover.commons.graph.local.ProductInspection
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy
import ai.acyclic.prover.commons.util.SrcPosition

trait Traceable extends Product {

  object explain {

    private val node = Traceable.Inspection.node(Traceable.this)

    def nodeText = node.nodeText
    def treeText = {
      val viz = LinkedHierarchy.Default.visualiseNode(node)
      viz.toString
    }

  }

//  final def explain: Traceable.SemilatticeRepr.node = {
//
//    val node = Traceable.SemilatticeRepr.node(explainView)
//
//    node
//  }
}

object Traceable {

  object Inspection extends ProductInspection[Product, Traceable] {}

  trait BySrc extends Traceable with Product1[String] {

    {
      definedAt
    }
    final lazy val definedAt: SrcPosition = _definedAt

    protected def _definedAt: SrcPosition = {

      val thisClass = classOf[BySrc]
      val stack = CallStackRef
        .below(
          1,
          condition = { v =>
            v.isUnderClasses(thisClass)
          }
        )
        .pop { v =>
          v.isLazyCompute || v.isInit
        }

      SrcPosition.Runtime(stack)
    }

    lazy val _1 = definedAt.toString

    override def canEqual(that: Any): Boolean = that.isInstanceOf[BySrc]
  }
}
