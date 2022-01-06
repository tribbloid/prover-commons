package ai.acyclic.graph.commons.viz

import ai.acyclic.graph.commons.diff.StringDiff
import ai.acyclic.graph.commons.reflect.format.TypeFormat
import ai.acyclic.graph.commons.{Padding, TextBlock, TreeFormat, TreeLike}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.language.implicitConversions

trait TypeOfs extends TypeVizSystem {

  import reflection._

  class TypeOf[T](
      val tt: universe.Type
  ) {
    val exe: Execution = Execution()
    import exe._

    type TT = T

    lazy val typeView: TypeView = reflection.typeView(tt)
    lazy val typeTree: exe.TypeVizTree = TypeVizTree(typeView)

    //  def nodeStr: String = tree.nodeStr
    //  def treeStr: String = tree.treeString

    override def toString: String = {

      typeTree.treeString
    }

    def should_=:=(that: TypeOf[_] = null): Unit = {

      val Seq(s1, s2) = Seq(this, that).map { v =>
        Option(v).map(_.typeTree.treeString)
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

    def =!=(that: TypeOf[_] = null): Unit = should_=:=(that)
  }

  object TypeOf {

    def withFormat(format: TypeFormat) = new WithFormat(format)

    implicit def asTree(v: TypeOf[_]): v.exe.TypeVizTree = v.typeTree

  }

  case class Execution() {

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

    case class TypeVizTree(
        node: TypeView,
        visited: mutable.ArrayBuffer[TypeID] = newTCache,
        expanded: Expanded = Expanded()
    ) extends TreeLike {

      override lazy val treeFormat: TreeFormat = format.treeFormat

      import node._

      {
        visited += node.id
      }

      val baseTypes_NoSelf: List[TypeView] = baseTypes.filterNot(tt => tt.id == node.id)

      // all eager execution ends here

      def expansionHistory: expanded.Record = expanded.apply(node.id)

      //    case object Strings {

      lazy val formatting: reflection.TypeIR = node.formattedBy(format.base)

      lazy val typeStr: String = {

        val result = node.formattedBy(format.base)

        result.text
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

        if (!format.showArgTree) {
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

      case class ArgTree(node: TypeView) extends TreeLike {

        override lazy val children: List[TypeVizTree] = Seq(node)
          .flatMap(_.args)
          .map { tt =>
            val result = TypeVizTree.this.copy(tt, visited = newTCache)

            result
          }
          .toList

        override lazy val nodeString: String = {

          val ttStr = node.self.typeConstructor.toString

          val size = children.size

          if (size == 1) s"$ttStr [ $size ARG ] :"
          else if (size == 0) s"$ttStr [ No ARG ]"
          else s"$ttStr [ $size ARGS ] :"
        }
      }

      lazy val argTrees: Seq[ArgTree] = {

        val forms = formatting.EquivalentTypes.recursively

        val trees = forms
          .map { ff =>
            ff.typeView
          }
          .distinct
          .map { v =>
            ArgTree(v)
          }

        val result = trees
          .filter { tree =>
            tree.children.nonEmpty
          }

        result
      }

      object Children {

        val history: expanded.Record = {
          val result = expansionHistory
          result.++()
          result
        }

        lazy val expandBaseTrees: List[TypeVizTree] = {

          def list = baseTypes_NoSelf.flatMap { node =>
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
            for (args <- argTrees; tree <- args.children) {

              tree.Children.expandAll
            }

            argTrees
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
