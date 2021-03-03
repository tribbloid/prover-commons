package com.tribbloids.graph.commons.util.viz

import com.tribbloids.graph.commons.util.ScalaReflection._
import com.tribbloids.graph.commons.util.TextBlock.Padding
import com.tribbloids.graph.commons.util.diff.StringDiff
import com.tribbloids.graph.commons.util.reflect.{TypeFormat, TypeID, TypeView}
import com.tribbloids.graph.commons.util.{TextBlock, TreeFormat, TreeLike}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.language.implicitConversions

case class VizType(
    tt: universe.Type,
    format: TypeFormat = TypeFormat()
) {

  import VizType._
  val exe: Execution = Execution(format)
  import exe._

  lazy val tree: exe.Tree = Tree(TypeView(tt))

  //  def nodeStr: String = tree.nodeStr
  //  def treeStr: String = tree.treeString

  override def toString: String = {

    tree.treeString
  }

  def shouldBe(that: VizType = null): Unit = {

    val diff = StringDiff(Option(this).map(_.toString), Option(that).map(_.toString), Seq(this.getClass))

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
}

trait VizType_Imp0 extends TypeFormat.Default.WithFormat {

  def withFormat(format: TypeFormat) = new format.WithFormat
}

object VizType extends VizType_Imp0 {

  implicit def asTree(v: VizType): v.exe.Tree = v.tree

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

      val argsOpt: Option[Args.type] = {
        if (Args.children.isEmpty) None
        //      else if (expandRecord.count >= 2) None
        else {
          Some(Args)
        }
      }

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

        if (!typeFormat.showArgTree) {
          ""
        } else {

          val raw = Children.expandArgs
            .map { tree =>
              tree.treeString
            }

          val indented = raw.map { tt =>
            "\n" + TextBlock(tt)
              .padLeft(Padding("      `", "       "))
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

        override lazy val children: List[Tree] = display.variants
          .flatMap(_.typeArgs)
          .distinct
          .map { tt =>
            val result = copy(TypeView(tt), visited = newTCache)

            result
          }
          .toList

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

  }
}
