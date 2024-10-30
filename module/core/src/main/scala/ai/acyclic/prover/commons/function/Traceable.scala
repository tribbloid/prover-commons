package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.debug.CallStackRef
import ai.acyclic.prover.commons.graph.local.ProductInspection
import ai.acyclic.prover.commons.graph.viz.{Hierarchy, LinkedHierarchy}
import ai.acyclic.prover.commons.util.SrcPosition

trait Traceable extends Product {

  object explain {

    private val node = Traceable.Inspection.node(Traceable.this)

    def nodeText: String = node.nodeText

    def text_hierarchy: String = {

      val viz = Hierarchy.Default.visualiseNode(node)
      viz.toString
    }

    def text_linkedHierarchy: String = {
      val viz = LinkedHierarchy.Default.visualiseNode(node)
      viz.toString
    }
  }
}

object Traceable {

  object Inspection extends ProductInspection[Product, Traceable] {

    final override val node: Product => ai.acyclic.prover.commons.function.Traceable.Inspection.BuiltIn = { v =>
      BuiltIn(v)
    }
  }
//  object Inspection extends Local.Semilattice.Upper.Inspection[Any] {
//
//    object Underlying extends ProductInspection[Product, Product] {}
//
//    override protected type node = Underlying.node
//    override val node: Any => node = {
//
//      case v: Traceable =>
//      case v: Product   =>
//
//    }
//  }

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

//    final override lazy val productPrefix = definedAt.toString

    final lazy val _1 = definedAt.toString

    override def canEqual(that: Any): Boolean = {
      that match {
        case _: BySrc => true
        case _        => false
      }
    }
  }
}
