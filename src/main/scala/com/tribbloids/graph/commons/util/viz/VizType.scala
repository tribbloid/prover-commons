package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.ScalaReflection.universe
import com.tribbloids.graph.commons.util.{TreeFormat, TreeLike}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.language.implicitConversions

case class VizType(tt: universe.Type) {

  import VizType._

  lazy val tree: Tree = Tree(Node(tt))

//  def nodeStr: String = tree.nodeStr
//  def treeStr: String = tree.treeString

  override def toString: String = {

    tree.treeString
  }

  def shouldBe(that: VizType): Unit = {

    Predef.assert(
      this.tt =:= that.tt,
      s"""
         |${this.tt}
         |!=
         |${that.tt}
         |""".stripMargin.trim
    )
  }
}

trait VizType_Imp0 {

  import universe._

  def apply[T](implicit ev: WeakTypeTag[T]): VizType = VizType(ev.tpe)

  def infer[T](v: T)(implicit ev: WeakTypeTag[T]): VizType = apply[T]
}

object VizType extends VizType_Imp0 {

  implicit def asTree(v: VizType): Tree = v.tree

  case class TypeID(
      symbol: universe.Symbol,
      str: String
  )

  def getID(_type: universe.Type): TypeID = TypeID(_type.typeSymbol, _type.toString)

  import universe._

  case class Expanded() {

    @volatile var hide: Int = 0

    val ordinal = new AtomicInteger(0)

    case class Record(
        visibleCount: AtomicInteger = new AtomicInteger(0),
        hiddenCount: AtomicInteger = new AtomicInteger(0)
    ) {

      def count: Int = visibleCount.get() + hiddenCount.get()

      lazy val refString: String = {

        val v = ordinal.getAndIncrement().toString
        v
      }

      def ++ : Unit = {
        if (hide > 0) hiddenCount.incrementAndGet()
        else visibleCount.incrementAndGet()
      }
    }

    val records: mutable.HashMap[TypeID, Record] = mutable.HashMap.empty // Type -> Ordinal / Count

    def apply(id: TypeID): Record = records.getOrElseUpdate(id, Record())

//    def +=(id: TypeID): Unit = {
//
//      val count = apply(id)
//
//      count.count += 1
//    }
  }

  case class Node(
      _type: Type,
      comment: Option[String] = None,
  ) {

    lazy val typeID: TypeID = getID(_type)

    lazy val show1Line: String = {

      val base = _type.toString

//      val base = _type.typeSymbol.name.toString // TODO: what are my options?

      (Seq(base) ++ comment).mkString(" *** ")
    }

    override def toString: String = show1Line
  }

  def emptyVisited: mutable.ArrayBuffer[TypeID] = mutable.ArrayBuffer.empty

  case class Tree(
      node: Node,
      visited: mutable.ArrayBuffer[TypeID] = emptyVisited,
      expanded: Expanded = Expanded(),
      showArgs: Boolean = true,
      override val format: TreeFormat = TreeFormat.Indent2
  ) extends TreeLike {

    import node._

    {
      visited += node.typeID
    }

    val baseNodes: List[Node] = {
//      if (expandRecord.count >= 2) {
//
//        Nil
//      } else {

      val baseClzs = _type.baseClasses

      val baseNodes = baseClzs.map { clz =>
        val tt = _type.baseType(clz)
        if (tt == NoType) Node(tt, Some(clz.toString))
        else Node(tt)
      }

      baseNodes
//      }
    }

    val sameNodes: List[Node] = baseNodes.filter(tt => tt._type =:= _type)

    val baseNodes_NoSelf: List[Node] = baseNodes.filterNot(tt => tt.typeID == node.typeID)

    val argsOpt: Option[Args.type] = {
      if (Args.children.isEmpty) None
//      else if (expandRecord.count >= 2) None
      else {
        Some(Args)
      }
    }

    // all eager execution ends here

    def expansionHistory: expanded.Record = expanded(node.typeID)

//    case object Strings {

    def typeStr: String = {

      val nameStr = node.show1Line
      val sameTypeStrs = sameNodes.map(_.show1Line)
      val str = (Seq(nameStr) ++ sameTypeStrs).distinct.mkString(" =:= ")

      str
    }

    def refStr: String = {
      val history: expanded.Record = expansionHistory
      val ref = if (history.visibleCount.get() >= 2) {

        val refFillLength = Math.max(5, 120 - typeStr.length)
        val refFill = Array.fill(refFillLength)(".").mkString

        val i = history.refString
        s" $refFill [$i]"
      } else {
        ""
      }

      ref
    }

    def argTreeStr: String = {

      if (!showArgs) {
        ""
      } else {

        val raw = Children.expandArgs
          .map { tree =>
            tree.treeString
          }

        val indented = raw.map { tt =>
          "\n" + format
            .wText(tt)
            .prepend(
              "       `",
              "        "
            )
            .build
        }

        indented.getOrElse("")
      }
    }
//    }

    override def nodeString: String = {

      Children.expandAll

      typeStr + refStr + argTreeStr
    }

    object Args extends TreeLike {

      override lazy val children: List[Tree] = _type.typeArgs.map { tt =>
        val result = copy(Node(tt), visited = emptyVisited)

        result
      }

      override lazy val nodeString: String = {

        val size = children.size

        if (size == 1) s"[ $size ARG ] :"
        else if (size == 0) "[ No ARG ]"
        else s"[ $size ARGS ] :"
      }
    }

    object Children {

      val history: expanded.Record = {
        val result = expansionHistory
        result.++
        result
      }

      lazy val expandBaseTrees: List[Tree] = {

        def list = baseNodes_NoSelf.flatMap { node =>
          if (visited.contains(node.typeID)) None
          else {
            val tree = copy(node)

            tree.Children.expandBaseTrees

            Some(tree)
          }
        }

        val result = if (history.count >= 2) {
          expanded.hide += 1
          list
          expanded.hide -= 1

          Nil
        } else {
          list
        }

        result
      }

      lazy val expandArgs: Option[Args.type] =
        if (history.count >= 2) {
          None
        } else {
          for (args <- argsOpt; tree <- args.children) {

            tree.Children.expandAll
          }

          argsOpt
        }

      lazy val expandAll: Unit = {
        expandBaseTrees
        expandArgs

//        for (args <- expandArgs; tree <- args.children) {
//
//          tree.Children.expandAll
//        }

        expandBaseTrees.foreach { tree =>
          tree.Children.expandAll
        }
      }
    }

    override lazy val children: List[TreeLike] = {

      val result = Children.expandBaseTrees

      result
    }
  }

  object Strong {

    def apply[T](implicit ev: TypeTag[T]): VizType = VizType(ev.tpe)

    def infer[T](v: T)(implicit ev: TypeTag[T]): VizType = apply[T]
  }

}
