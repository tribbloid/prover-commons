package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.diff.StringDiff
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.LinkedHierarchy
import ai.acyclic.prover.commons.refl.HasReflection
import ai.acyclic.prover.commons.typesetting.{Padding, TextBlock}

import scala.language.implicitConversions

object TypeOfMixin {

  trait VNodeLike {

    lazy val argDryRun: Unit = {}
  }
}

trait TypeOfMixin extends HasReflection {

  import reflection.*

  val format: TypeHierarchy

  object TypeOf {

    implicit def asNodes(v: TypeOf[?]): v.TypeRefBatch#Nodes = v.TypeRefBatch().nodes

    implicit def asShow(v: TypeOf[?]): v.TypeRefBatch#ShowType = v.TypeRefBatch().ShowType()
  }

  class TypeOf[T](
      //  TODO: this API won't work for multiple types (e.g. showing common join & meet in heyting algebra)
      //   it should be replaced by a more general construct
      val tt: universe.Type
  ) {

    lazy val typeOps: TypeOps = TypeOps(reflection.typeView(tt))

    case class TypeRefBatch(
        override val vizGroup: format.DelegateFormat.Group = format.DelegateFormat.Group()
    ) extends TypeRefBindings {

      lazy val nodes: Nodes = Nodes(typeOps.formattedBy(format.typeFormat))

      lazy val typeStr: String = nodes.typeText

      lazy val graph: Local.Diverging.Graph[TypeRefBindings.node] = {
        Local.Diverging.Graph.makeExact(nodes.SuperTypeNode)

        /**
          * CAUTION: [[nodes.SuperTypeNode]] is a semilattice node in Heyting algebra.
          *
          * BUT if counting reference it becomes a diverging graph, potentially with cycle.
          *
          * which is why it returned a [[Local.Diverging.Graph]] constructed from
          * [[Local.Diverging.UpperSemilattice.Node]]
          */
      }

      case class ShowType() extends Local.VisualOps(graph, vizGroup)
    }

    lazy val typeStr = {
      TypeRefBatch().typeStr
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

    val vizGroup: format.DelegateFormat.Group

    case class Nodes(
        ir: TypeIR
    ) {

      val node: TypeOps = ir.typeOps

      lazy val argGraph: Local.Diverging.Graph[TypeRefBindings.node] = {

        val equivalentIRs = ir.EquivalentTypes.recursively

        val argNodes = equivalentIRs
          .groupBy(_.typeOps)
          .flatMap {
            case (_, vs) =>
              val argNode = this.copy(ir = vs.head).ArgNode
              if (argNode.inductions.isEmpty) None
              else Some(argNode)
          }
          .toSeq

        Local.Diverging.Graph(argNodes*)
      }

      lazy val typeText: String = ir.text

      lazy val argViz: vizGroup.Viz[TypeRefBindings.node] = {
        vizGroup.Viz(argGraph)
      }

      lazy val argText: String = {

        argViz.toString
      }

      lazy val fullText: String = {

        if (!format._showArgs || argGraph.isEmpty) {
          typeText
        } else {

          val indentedArgText =
            TextBlock(argText).pad
              .left(Padding.argLeftBracket)
              .indent("      ")
              .build

          typeText + "\n" + indentedArgText
        }
      }

      case object SuperTypeNode extends node {

        final override lazy val identityC: Some[TypeOfMixin.this.reflection.TypeID] = Some(node.reference)

        override val inductions: List[(Arrow.`~>`, node)] = {

          node.superTypes_nonTransitive
            .filter { tv =>
              tv.toString != "Any" // skipped for being too trivial
            }
            .map { tv =>
              Nodes(TypeOps(tv).formattedBy(format.typeFormat)).SuperTypeNode
            }
        }

        override lazy val toString: String = fullText

        override lazy val argDryRun: Unit = {

          argViz.dryRunOnce
        }
      }

      case object ArgNode extends node {

        final override lazy val identityC: None.type = None

        override lazy val inductions: List[(Arrow.`~>`, node)] = {
          node.args.map { tt =>
            Nodes(TypeOps(tt).formattedBy(format.typeFormat)).SuperTypeNode
          }
        }

        override lazy val toString: String = {

          val ttStr = node.as.typeConstructor.toString

          val size = node.args.size

          if (size == 1) s"$ttStr [ $size ARG ] :"
          else if (size == 0) s"$ttStr [ No ARG ]"
          else s"$ttStr [ $size ARGS ] :"
        }

      }
    }
  }
}
