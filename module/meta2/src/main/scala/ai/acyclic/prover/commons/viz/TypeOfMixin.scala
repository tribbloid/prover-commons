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

    def argDryRun(): Unit
  }
}

trait TypeOfMixin extends HasReflection {

  import reflection.*

  val format: TypeHierarchy

  class TypeOf[T](
      //  TODO: this API won't work for multiple types (e.g. showing common join & meet in heyting algebra)
      //   it should be replaced by a more general construct
      val tt: universe.Type
  ) {

    lazy val typeOps: TypeOps = TypeOps(reflection.typeView(tt))

    lazy val vizGroup: VisualisationGroup = VisualisationGroup()
    lazy val nodes: vizGroup.Nodes = vizGroup.Nodes(typeOps.formattedBy(format.typeFormat))

    lazy val typeStr: String = nodes.typeText

    lazy val graph = Local.AnyGraph
      .Outbound(nodes.SuperTypeNode)

    lazy val text_hierarchy: format.DelegateFormat.Group#Viz[VisualisationGroup.node] =
      graph.text_linkedHierarchy(vizGroup.delegateGroup)

    lazy val text_flow =
      graph.text_flow(Flow.Default)

    override def toString: String = {

      text_hierarchy.toString
    }

    def should_=:=(that: TypeOf[?] = null): Unit = {

      val Seq(s1, s2) = Seq(this, that).map { v =>
        Option(v).map(_.text_hierarchy.toString)
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

  object TypeOf {

    implicit def asNodes(v: TypeOf[?]): v.vizGroup.Nodes = v.nodes
  }

  object VisualisationGroup extends Local.AnyGraph.Outbound.NodeGroup {

    // technically this layer could be collapsed into GraphRepr
    trait node extends NodeInGroup with TypeOfMixin.VNodeLike {}
  }

  // visualisations in the same group should not display redundant information
  case class VisualisationGroup() {

    import VisualisationGroup.*

    lazy val delegateGroup: format.DelegateFormat.Group = format.DelegateFormat.Group()

    case class Nodes(
        ir: TypeIR
    ) {

      val node: TypeOps = ir.typeOps

      lazy val argGraph: Local.AnyGraph.Outbound[VisualisationGroup.node] = {

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

        Local.AnyGraph.Outbound(argNodes*)
      }

      lazy val typeText: String = ir.text

      lazy val argViz: delegateGroup.Viz[VisualisationGroup.node] = delegateGroup.Viz(argGraph)

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

        override def argDryRun(): Unit = {

          argViz.dryRun()
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

        override def argDryRun(): Unit = {}
      }
    }
  }
}
