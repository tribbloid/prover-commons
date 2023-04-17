//package ai.acyclic.prover.commons.graph.local.impl
//
//import ai.acyclic.prover.commons.HasOuter
//import ai.acyclic.prover.commons.graph.Topology.SemilatticeT
//import ai.acyclic.prover.commons.graph.local.Semilattice
//import ai.acyclic.prover.commons.reflect.ScalaReflection
//import ai.acyclic.prover.commons.viz.text.{Padding, TextBlock}
//
//import scala.reflect.ClassTag
//
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
//  case class FallbackOps(value: Any) extends Ops {
//
//    final override protected def getNodeText = value.toString
//
//    final override protected def getInduction = Nil
//  }
//
//  case class ProductOps(value: Product) extends Ops {
//
//    protected lazy val contains: List[Any] = value.productIterator.toList
//
//    protected lazy val _args_children = contains.map {
//      case v: Include =>
//        Right(v)
////      case v: Arg     => Left(v)
//      case v @ _ =>
//        Left(v)
//    }
//
//    protected lazy val _args = _args_children.collect {
//      case Left(v) => v
//    }
//
//    protected lazy val _children = _args_children.collect {
//      case Right(v) => v
//    }
//
//    lazy val constructorString: String = {
//
//      val hasOuter = value.getClass.getDeclaringClass != null
//
//      if (hasOuter) {
//        val list = HasOuter.outerListOf(value)
//
//        val names = list.map { v =>
//          val dec = decodedStrOf(v)
//
//          dec
//        }
//
//        names.reverse.mkString(" ‣ ")
//      } else {
//        decodedStrOf(value)
//      }
//    }
//
//    final override protected def getNodeText = {
//
//      if (_args.isEmpty) {
//
//        constructorString
//      } else {
//
//        val _notTree = _args.map { str =>
//          TextBlock("" + str).pad.left(Padding.argLeftBracket).build
//        }
//
//        TextBlock(constructorString)
//          .zipRight(
//            TextBlock(_notTree.mkString("\n"))
//          )
//          .build
//      }
//    }
//
//    final override protected def getInduction = {
//
//      _children.map(v => Ops(v))
//    }
//  }
//}
//
//object ProductDiscovery {
//
//  trait Exclude
//
//  def decodedStrOf(v: Any): String = {
//    val clz = v.getClass
//    val enc =
//      clz.getCanonicalName.replace(clz.getPackage.getName, "").stripPrefix(".").stripSuffix("$")
//
//    val dec = ScalaReflection.universe.TypeName(enc).decodedName
//    dec.toString
//  }
//}
