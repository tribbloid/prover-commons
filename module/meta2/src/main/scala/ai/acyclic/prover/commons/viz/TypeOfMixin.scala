package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.diff.StringDiff
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.Flow
import ai.acyclic.prover.commons.refl.HasReflection
import ai.acyclic.prover.commons.typesetting.{Padding, TextBlock}

import scala.language.implicitConversions

object TypeOfMixin {

  trait VNodeLike {

    lazy val scanArgsOnce: Unit = {}
  }
}

trait TypeOfMixin extends HasReflection {

  import reflection.*

  val treeFormat: TypeTreeFormat

  object TypeOf {

    implicit def asNodes(v: TypeOf[?]): v.TypeRefBatch#TypeIRView = v.TypeRefBatch().ir
  }

  class TypeOf[T](
      //  TODO: this API won't work for multiple types (e.g. showing common join & meet in heyting algebra)
      //   it should be replaced by a more general construct
      val tt: universe.Type
  ) {

    lazy val typeOps: TypeOps = TypeOps(reflection.typeView(tt))

    case class TypeRefBatch(
        override val vizGroup: treeFormat._LinkedHierarchyFormat.Group = treeFormat._LinkedHierarchyFormat.Group()
    ) extends TypeRefBindings {

      lazy val ir: TypeIRView = TypeIRView(typeOps.formattedBy(treeFormat.typeFormat))

      lazy val typeStr: String = ir.typeText

      lazy val node: ir.SuperTypeNode.type = ir.SuperTypeNode

      lazy val graph: Local.Diverging.Graph[TypeRefBindings.node] = {

        /**
          * CAUTION: [[irView.SuperTypeNode]] is a semilattice node in Heyting algebra.
          *
          * BUT if counting reference it becomes a diverging graph, potentially with cycle.
          *
          * which is why it returned a [[Local.Diverging.Graph]] constructed from
          * [[Local.Diverging.UpperSemilattice.Node]]
          */
        Local.Diverging.Graph.makeExact(node)
      }
    }

    lazy val typeStr: String = {
      TypeRefBatch().typeStr
    }

    def text_linkedHierarchy(
        vizGroup: treeFormat._LinkedHierarchyFormat.Group = treeFormat._LinkedHierarchyFormat.Group()
    ): treeFormat._LinkedHierarchyFormat.Visual = {
      vizGroup.Viz(TypeRefBatch(vizGroup).graph)
    }

    def text_flow(
        vizGroup: treeFormat._LinkedHierarchyFormat.Group = treeFormat._LinkedHierarchyFormat.Group()
    ): Flow.Default.Visual = {
      TypeRefBatch(vizGroup).node.text_flow(
        treeFormat._FlowFormat
      )
    }

    override def toString: String = {

      this.text_linkedHierarchy().toString
    }

    def should_=:=(that: TypeOf[?] = null): Unit = {

      val Seq(s1, s2) = Seq(this, that).map { v =>
        Option(v).map(_.text_linkedHierarchy().toString)
      }

      val diff = StringDiff(s1, s2, Seq(this.getClass))

      (diff.Left.isDefined, diff.Right.isDefined) match {

        case (true, true) =>
          Predef.assert(
            this.tt =:= that.tt,
            diff.ErrorDiffClue
          )

        case _ =>
          diff.show()
      }
    }

    def =!=(that: TypeOf[?] = null): Unit = should_=:=(that)
  }

  object TypeRefBindings extends Local.Diverging.UpperSemilattice.Codomain {

    // technically this layer could be collapsed into GraphRepr
    trait node extends Node_ with TypeOfMixin.VNodeLike {}
  }

  // visualisations in the same group should not display redundant information
  trait TypeRefBindings { // TODO: only 1 subclass, no need to be a layer of abstraction

    import TypeRefBindings.*

    val vizGroup: treeFormat._LinkedHierarchyFormat.Group

    case class TypeIRView(
        ir: TypeIR
    ) {

      val node: TypeOps = ir.typeOps
      lazy val typeText: String = ir.text

      case object SuperTypeNode extends node {

        final override lazy val identityC: Some[TypeOfMixin.this.reflection.TypeID] = Some(node.id_deAlias)

        override val inductions: List[(Arrow.`~>`, node)] = {

          if (node.isBuiltIn) {
            Nil
          } // do not expand built-in types
          else {

            node.superTypes_nonTransitive
              .filter { tv =>
                tv.toString != "Any" // skipped for being too trivial
              }
              .map { tv =>
                TypeIRView(TypeOps(tv).formattedBy(treeFormat.typeFormat)).SuperTypeNode
              }
          }
        }

        private lazy val dependentArgs = Args.nonEmpty

        override lazy val scanArgsOnce: Unit = {
          dependentArgs.foreach { args =>
            args.visual.scanLinks
          }
        }

        override lazy val nodeText: String = {
          dependentArgs
            .map { args =>
              val indentedArgText =
                TextBlock(args.visual.toString).pad
                  .left(Padding.argLeftBracket)
                  .indent("      ")
                  .build

              typeText + "\n" + indentedArgText
            }
            .getOrElse(typeText)
        }
      }

      case object ArgNode extends node {

        final override lazy val identityC: None.type = None

        override lazy val inductions: List[(Arrow.`~>`, node)] = {
          node.args.map { tt =>
            TypeIRView(TypeOps(tt).formattedBy(treeFormat.typeFormat)).SuperTypeNode
          }
        }

        override lazy val nodeText: String = {

          val ttStr = node.as.typeConstructor.toString

          val size = node.args.size

          if (size == 1) s"$ttStr [ $size ARG ] :"
          else if (size == 0) s"$ttStr [ No ARG ]"
          else s"$ttStr [ $size ARGS ] :"
        }

      }

      object Args {

        lazy val graph: Local.Diverging.Graph[TypeRefBindings.node] = {

          val equivalentIRs = ir.EquivalentTypes.recursively

          val argNodes = equivalentIRs
            .groupBy(_.typeOps)
            .flatMap {
              case (_, vs) =>
                val argNode = TypeIRView.this.copy(ir = vs.head).ArgNode
                if (argNode.inductions.isEmpty) None
                else Some(argNode)
            }
            .toSeq

          Local.Diverging.Graph.makeExact(argNodes*)
        }

        lazy val nonEmpty: Option[Args.type] = {
          if (!treeFormat._showArgs || graph.isEmpty)
            None
          else
            Some(this)
        }

        lazy val visual: vizGroup.Viz = {
          vizGroup.Viz(graph)
        }
      }
    }
  }
}
