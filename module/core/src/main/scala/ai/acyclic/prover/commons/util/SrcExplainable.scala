package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.cap.Capabilities
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.Hierarchy

import scala.runtime.ScalaRunTime

trait SrcExplainable extends SrcTraceable {
  import SrcExplainable._

  @transient lazy val explain: Explain = Explain(this)

  override def toString: String = {
    explain.nodeText
  }
}

/**
  * can mixin [[Capabilities.Capability]], but so far, the only [[Capability]] is for refining candidates of polymorphic
  * cases
  */
object SrcExplainable extends Capabilities {
  // TODO: can this be generalised, not just for functions?

  case class Explain private (value: SrcExplainable) extends Local.Tree.NodeImpl[SrcExplainable] {

    override def getNodeText: String = {

      val body = value match {

        case v: DecodedName =>
          RuntimeClass.Decoded.simpleNameOf(v)
        case v: Product =>
          ScalaRunTime._toString(v)
        case _ =>
          s"${value.definedAt.toString}"
      }

      body
    }

    override protected def getInduction: Seq[(_Arrow, Explain)] = {

      val children = value match {
        case v: Composite => v.composedFrom
        case _            => Nil
      }

      children.map(v => Explain(v))
    }

    lazy val treeText: String = {

      val viz = Hierarchy.default.apply(this.mkTree)
      viz.treeText
    }

    override def toString: String = treeText
  }

  ////

  trait Composite extends SrcExplainable {

    def composedFrom: Seq[SrcExplainable]
  }

  trait Composite1 extends Composite {

    def backbone: SrcExplainable

    @transient final lazy val composedFrom = Seq(backbone)
  }

  trait DecodedName extends SrcExplainable {}

}
