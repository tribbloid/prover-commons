package ai.acyclic.prover.commons.graph.local.impl

import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Semilattice
import ai.acyclic.prover.commons.reflect.ScalaReflection
import ai.acyclic.prover.commons.viz.text.{Padding, TextBlock}
import ai.acyclic.prover.commons.HasOuter
import ai.acyclic.prover.commons.graph.Topology.SemilatticeT

import scala.reflect.ClassTag

abstract class ProductDiscovery[Include](
    implicit
    ev: ClassTag[Include]
) extends Semilattice.Upper[Any] {

  def root: Any

  import ProductDiscovery._

  trait Ops extends SemilatticeT.UpperT.Ops[Any] {}
  override lazy val Ops: Any => Ops = {
    case v: Exclude =>
      FallbackOps(v)
    case v: Product =>
      ProductOps(v)
    case v @ _ =>
      FallbackOps(v)
  }

  case class FallbackOps(node: Any) extends Ops {

    final override protected def getNodeText = node.toString

    final override protected def getInduction: Seq[(Arrow.`~>`.^, Product)] = Nil
  }

  case class ProductOps(node: Product) extends Ops {

    protected lazy val contains: List[Any] = node.productIterator.toList

    protected lazy val _args_children = contains.map {
      case v: Include =>
        Right(v)
//      case v: Arg     => Left(v)
      case v @ _ =>
        Left(v)
    }

    protected lazy val _args = _args_children.collect {
      case Left(v) => v
    }

    protected lazy val _children = _args_children.collect {
      case Right(v) => v
    }

    lazy val constructorString: String = {

      val hasOuter = node.getClass.getDeclaringClass != null

      if (hasOuter) {
        val list = HasOuter.outerListOf(node)

        val names = list.map { v =>
          val dec = decodedStrOf(v)

          dec
        }

        names.reverse.mkString(" ‣ ")
      } else {
        decodedStrOf(node)
      }
    }

    final override protected def getNodeText = {

      if (_args.isEmpty) {

        constructorString
      } else {

        val _notTree = _args.map { str =>
          TextBlock("" + str).pad.left(Padding.argLeftBracket).build
        }

        TextBlock(constructorString)
          .zipRight(
            TextBlock(_notTree.mkString("\n"))
          )
          .build
      }
    }

    final override protected def getInduction: Seq[(Arrow.`~>`.^, Any)] = {

      _children
    }
  }
}

object ProductDiscovery {

  trait Exclude

  def decodedStrOf(v: Any): String = {
    val clz = v.getClass
    val enc =
      clz.getCanonicalName.replace(clz.getPackage.getName, "").stripPrefix(".").stripSuffix("$")

    val dec = ScalaReflection.universe.TypeName(enc).decodedName
    dec.toString
  }
}
