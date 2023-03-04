package ai.acyclic.prover.commons.meta2

import ai.acyclic.prover.commons.HasOuter
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.typesetting.{Padding, TextBlock}

import scala.reflect.ClassTag

object ProductDiscovery {

  trait Exclude

  def decodedStrOf(v: Any): String = {
    val clz = v.getClass
    val enc =
      clz.getCanonicalName.replace(clz.getPackage.getName, "").stripPrefix(".").stripSuffix("$")

    val dec = ScalaReflection.universe.TypeName(enc).decodedName
    dec.toString
  }

  trait Node extends Local.Semilattice.Upper.NodeImpl[Any] with Product {}

  case class FallbackOps(value: Any) extends Node {

    final override protected def nodeTextC = value.toString
//
    final override protected def inductionC = Nil

  }

  case class ProductOps[ACCEPT](value: Product)(
      implicit
      ev: ClassTag[ACCEPT]
  ) extends Node {

    protected lazy val contains: List[Any] = value.productIterator.toList

    protected lazy val _args_children = contains.map {
      case v: ACCEPT =>
        Right(v)
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

      val hasOuter = value.getClass.getDeclaringClass != null

      if (hasOuter) {
        val list = HasOuter.outerListOf(value)

        val names = list.map { v =>
          val dec = decodedStrOf(v)

          dec
        }

        names.reverse.mkString(" ‣ ")
      } else {
        decodedStrOf(value)
      }
    }

    final override protected def nodeTextC = {

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

    final override protected def inductionC = {

      _children.map { v =>
        apply[ACCEPT](v)
      }
    }
  }

  def apply[A: ClassTag](v: A): Node = {
    v match {
      case v: Exclude =>
        FallbackOps(v)
      case v: Product =>
        ProductOps[A](v)
      case v @ _ =>
        FallbackOps(v)
    }
  }

}

//abstract class ProductDiscovery[Include](
//    implicit
//    ev: ClassTag[Include]
//) extends Semilattice.Upper[Any] {
//
//  def root: Any
//
//  import ProductDiscovery._
//
//  trait Ops extends SemilatticeT.UpperT._Node[Any] {}
//  override lazy val Ops: Any => Ops = {
//    case v: Exclude =>
//      FallbackOps(v)
//    case v: Product =>
//      ProductOps(v)
//    case v @ _ =>
//      FallbackOps(v)
//  }
//
//}
