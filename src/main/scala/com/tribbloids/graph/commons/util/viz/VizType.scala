package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.ScalaReflection.universe
import com.tribbloids.graph.commons.util.TreeLike

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

case class VizType(tt: universe.Type) {

  import VizType._

  lazy val tree: Tree = Tree(Node(tt))

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

  case class Node(
      _type: Type,
      comment: Option[String] = None,
  ) {

    def show1Line: String = {
      //    tpe.typeArgs
      //
      //    tpe.typeParams

      //    s"$tpe \t:= ${showRaw(tpe.typeSymbol.typeSignature)}"

      (Seq(_type.toString) ++ comment).mkString(" *** ")
    }
  }

  case class Tree(
      node: Node,
      visited: mutable.Set[Type] = mutable.Set.empty,
      expanded: RefMap = RefMap()
  ) extends TreeLike {

    import node._

    {
      visited += _type
      expanded += _type
    }

    def refRecord: expanded.Record = expanded.records(_type.toString)

    lazy val typeStr: String = {

      val nameStr = node.show1Line
      val sameTypeStrs = sameNodes.map(_.show1Line)
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
        val result = copy(Node(tt), mutable.Set.empty)

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

    protected lazy val baseNodes: List[Node] = if (refRecord.count >= 2) {

      Nil
    } else {

      val baseClzs = _type.baseClasses

      val baseNodes = baseClzs.map { clz =>
        val tt = _type.baseType(clz)
        if (tt == NoType) Node(tt, Some(clz.toString))
        else Node(tt)
      }

      baseNodes
    }

    lazy val sameNodes: List[Node] = baseNodes.filter(tt => tt._type =:= _type)

//    lazy val superNodes: List[Node] = baseNodes.filterNot(tt => tt._type =:= _type)

    object Children {

      lazy val argTrees: List[ArgTree.type] = {

        ArgTree.opt.toList
      }

      lazy val baseTrees: List[Tree] = baseNodes.flatMap { tt =>
        if (visited.contains(tt._type)) None
        else {
          val result = copy(tt)

          // DepthFirstTraverse
          result.Children.renderBaseTrees

          Some(result)
        }
      }

      lazy val renderBaseTrees: Unit = baseTrees

      lazy val renderAll: Unit = {
        baseTrees.foreach { tree =>
          tree.Children.argTrees
        }
      }
    }

    override lazy val children: List[TreeLike] = {

      val result = Children.argTrees ++ Children.baseTrees

      result
    }
  }

  def apply[T](implicit ttag: TypeTag[T]): VizType = VizType(ttag.tpe)

  def infer[T: TypeTag](v: T): VizType = apply[T]
}
