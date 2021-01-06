package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.ScalaReflection.universe
import com.tribbloids.graph.commons.util.TreeLike

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

case class VizType(tt: universe.Type) {

  import VizType._

  lazy val tree: Tree = Tree(tt)

  override def toString: String = {

    tree.toString
  }
}

object VizType {

  import universe._

  case class RefMap() {

    val ordinal = new AtomicInteger(0)

    case class Record(
        @volatile var count: Int = 0
    ) {

      lazy val sid: String = {

        val v = ordinal.getAndIncrement().toString
        v
      }
    }

    val records: mutable.HashMap[String, Record] = mutable.HashMap.empty // Type -> Ordinal / Count

    def +=(_type: Type): Unit = {

      val count = records.getOrElseUpdate(_type.toString, Record())

      count.count += 1
    }
  }

  case class Tree(
      _type: Type,
      visited: mutable.Set[Type] = mutable.Set.empty,
      expanded: RefMap = RefMap()
  ) extends TreeLike {

    {
      visited += _type
      expanded += _type
    }

    def refRecord: expanded.Record = expanded.records(_type.toString)

    lazy val typeStr: String = {

      val nameStr = show1Line(_type)
      val sameTypeStrs = sameTypes.map(show1Line)
      val str = (Seq(nameStr) ++ sameTypeStrs).distinct.mkString(" =:= ")

      str
    }

    override lazy val nodeStr: String = {

      Children.renderAll

      val rr = refRecord
      val ref = if (rr.count >= 2) {

        val refFillLength = Math.max(5, 120 - typeStr.length)
        val refFill = Array.fill(refFillLength)(".").mkString

        val i = rr.sid
        s" $refFill [$i]"
      } else {
        ""
      }
      typeStr + ref
    }

    object ArgTree extends TreeLike {

      override val children: List[Tree] = _type.typeArgs.map { tt =>
        val result = copy(tt, mutable.Set.empty)

        // DepthFirstTraverse
        result.Children.renderAll

        result
      }

      override val nodeStr: String = {

        val size = children.size

        if (size == 1) s"  [ $size ARG ] :"
        else if (size == 0) "  [ No ARG ]"
        else s"  [ $size ARGS ] :"
      }

      lazy val opt: Option[ArgTree.type] = {
        if (ArgTree.children.isEmpty) None
        else {
          Some(ArgTree)
        }
      }
    }

    protected lazy val baseTypes: List[Type] = if (refRecord.count >= 2) {

      Nil
    } else {

      val baseClzs = _type.baseClasses

      val baseTypes = baseClzs.map { clz =>
        _type.baseType(clz)
      }

      baseTypes
    }

    lazy val sameTypes: List[universe.Type] = baseTypes.filter(tt => tt =:= _type)

    lazy val superTypes: List[universe.Type] = baseTypes.filterNot(tt => tt =:= _type)

    object Children {

      lazy val argTrees: List[ArgTree.type] = {

        ArgTree.opt.toList
      }

      lazy val superTrees: List[Tree] = superTypes.flatMap { tt =>
        if (visited.contains(tt)) None
        else {
          val result = copy(tt)

          // DepthFirstTraverse
          result.Children.renderSuperTrees

          Some(result)
        }
      }

      lazy val renderSuperTrees: Unit = superTrees

      lazy val renderAll: Unit = {
        superTrees.foreach { tree =>
          tree.Children.argTrees
        }
      }
    }

    override lazy val children: List[TreeLike] = {

      val result = Children.argTrees ++ Children.superTrees

      result
    }
  }

  def show1Line(tpe: Type): String = {
//    tpe.typeArgs
//
//    tpe.typeParams

//    s"$tpe \t:= ${showRaw(tpe.typeSymbol.typeSignature)}"

    tpe.toString
  }

  def apply[T](implicit ttag: TypeTag[T]): VizType = VizType(ttag.tpe)

  def infer[T: TypeTag](v: T): VizType = apply[T]
}
