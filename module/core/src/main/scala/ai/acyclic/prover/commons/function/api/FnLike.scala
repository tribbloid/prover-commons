package ai.acyclic.prover.commons.function.api

import ai.acyclic.prover.commons.cap.Capabilities
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.Hierarchy
import ai.acyclic.prover.commons.util.DefinedAtMixin

import scala.runtime.ScalaRunTime

trait FnLike extends DefinedAtMixin {
  import FnLike._

  private lazy val node = TreeNode(this)

  lazy val nodeText: String = {

    node.nodeText
  }

  lazy val treeText: String = {

    val viz = Hierarchy.default.apply(node.mkTree)
    viz.treeText
  }

  override def toString: String = {
    nodeText
  }
}

/**
  * can mixin [[Capabilities.Capability]], but so far, the only [[Capability]] is for refining candidates of polymorphic
  * cases
  */
object FnLike extends Capabilities {

  trait Transparent extends FnLike {

    def references: Seq[FnLike]
  }

  trait Transparent1 extends Transparent {

    def reference: FnLike

    @transient final lazy val references = Seq(reference)
  }

  trait Named extends FnLike {

    def name: String
  }

  case class TreeNode(value: FnLike) extends Local.Tree.NodeImpl[FnLike] {

    override def getNodeText: String = {

      val body = value match {

        case v: Named =>
          v.name
        case v: Product =>
          ScalaRunTime._toString(v)
        case _ =>
          s"${value.definedAt.toString}"

      }

      body
    }

    override protected def getInduction: Seq[(_Arrow, TreeNode)] = {

      val children = value match {
        case v: Transparent => v.references
        case _              => Nil
      }

      children.map(v => TreeNode(v))
    }
  }
}
