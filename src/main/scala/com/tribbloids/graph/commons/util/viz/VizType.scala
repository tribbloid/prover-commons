package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.ScalaReflection._
import com.tribbloids.graph.commons.util.TextBlock.Padding
import com.tribbloids.graph.commons.util.diff.StringDiff
import com.tribbloids.graph.commons.util.reflect.{TypeFormat, TypeID, TypeView}
import com.tribbloids.graph.commons.util.{TextBlock, TreeFormat, TreeLike}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.language.implicitConversions

class VizType(
    val tt: universe.Type,
    val format: TypeFormat = TypeFormat()
) {

  import VizType._
  val exe: Execution = Execution(format)
  import exe._

  lazy val typeTree: exe.Tree = Tree(TypeView(tt))

  //  def nodeStr: String = tree.nodeStr
  //  def treeStr: String = tree.treeString

  override def toString: String = {

    typeTree.treeString
  }

  def shouldBe(that: VizType = null): Unit = {

    val Seq(s1, s2) = Seq(this, that).map { v =>
      Option(v).map(_.typeTree.toString)
    }

    val diff = StringDiff(s1, s2, Seq(this.getClass))

    (diff.Left.isDefined, diff.Right.isDefined) match {

      case (true, true) =>
        Predef.assert(
          this.tt =:= that.tt,
          diff.errorStr
        )

      case _ =>
        diff.show()
    }
  }

  def =!=(that: VizType = null): Unit = shouldBe(that)
}

object VizType extends WithFormat(TypeFormat.Default) {

  def withFormat(format: TypeFormat) = new WithFormat(format)

  implicit def asTree(v: VizType): v.exe.Tree = v.typeTree

  case class Execution(
      typeFormat: TypeFormat = TypeFormat()
  ) {

    def newTCache: mutable.ArrayBuffer[TypeID] = mutable.ArrayBuffer.empty

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

        def ++(): Unit = {
          if (hide > 0) hiddenCount.incrementAndGet()
          else visibleCount.incrementAndGet()
        }
      }

      val records: mutable.HashMap[TypeID, Record] = mutable.HashMap.empty // Type -> Ordinal / Count

      def apply(id: TypeID): Record = records.getOrElseUpdate(id, Record())
    }

    case class Tree(
        node: TypeView,
        visited: mutable.ArrayBuffer[TypeID] = newTCache,
        expanded: Expanded = Expanded()
    ) extends TreeLike {

      override lazy val format: TreeFormat = typeFormat.treeFormat

      import node._

      {
        visited += node.id
      }

      val baseNodes: List[TypeView] = {

        val baseClzs = self.baseClasses

        val baseNodes = baseClzs.map { clz =>
          val tt = self.baseType(clz)
          if (tt == universe.NoType) TypeView(tt, Some(clz.toString))
          else TypeView(tt)
        }

        baseNodes
      }

      val baseNodes_NoSelf: List[TypeView] = baseNodes.filterNot(tt => tt.id == node.id)

//      val argsOpt: Option[ArgTree.type] = {
//        if (ArgTree.children.isEmpty) None
//        //      else if (expandRecord.count >= 2) None
//        else {
//          Some(ArgTree)
//        }
//      }

      // all eager execution ends here

      def expansionHistory: expanded.Record = expanded.apply(node.id)

      //    case object Strings {

      lazy val display: node.Display = node.Display(typeFormat)

      def typeStr: String = {

        val result = display.full

        result
      }

      def refStr: String = {
        val history: expanded.Record = expansionHistory
        val ref = if (history.visibleCount.get() >= 2) {

          val refFillLength = Math.max(5, 80 - typeStr.length)
//          val refFillLength = 3
          val refFill = Array.fill(refFillLength)(".").mkString

          val i = history.refString
          s" $refFill [$i]"
        } else {
          ""
        }

        ref
      }

      def argTreeStr: String = {

        if (!typeFormat.showArgTree) {
          ""
        } else {

          val raw = Children.expandArgs
            .map { tree =>
              tree.treeString
            }

          val indented = raw.map { tt =>
            "\n" + TextBlock(tt)
              .padLeft(Padding.argLeftBracket)
              .indent("      ")
              .build
          }

          indented.mkString("\n")
        }
      }
      //    }

      override def nodeString: String = {

        Children.expandAll

        typeStr + refStr + argTreeStr
      }

      case class ArgTree(_tt: Type) extends TreeLike {

        override lazy val children: List[Tree] = Seq(_tt)
          .flatMap(_.typeArgs)
          .map { tt =>
            val result = Tree.this.copy(TypeView(tt), visited = newTCache)

            result
          }
          .toList

        override lazy val nodeString: String = {

          val ttStr = _tt.typeConstructor.toString

          val size = children.size

          if (size == 1) s"$ttStr [ $size ARG ] :"
          else if (size == 0) s"$ttStr [ No ARG ]"
          else s"$ttStr [ $size ARGS ] :"
        }
      }

      object ArgTree {

        lazy val lists: Seq[ArgTree] = {
          display.variants
            .map { tt =>
              ArgTree(tt)
            }
            .filter { tree =>
              tree.children.nonEmpty
            }
        }

      }

      object Children {

        val history: expanded.Record = {
          val result = expansionHistory
          result.++()
          result
        }

        lazy val expandBaseTrees: List[Tree] = {

          def list = baseNodes_NoSelf.flatMap { node =>
            if (visited.contains(node.id)) None
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

        lazy val expandArgs: Seq[ArgTree] =
          if (history.count >= 2) {
            Nil
          } else {
            for (args <- ArgTree.lists; tree <- args.children) {

              tree.Children.expandAll
            }

            ArgTree.lists
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

  }
}
