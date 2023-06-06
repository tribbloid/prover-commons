package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.diff.StringDiff
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.typesetting.{Padding, TextBlock}

import scala.collection.mutable
import scala.language.implicitConversions

object TypeOfMixin {

  trait VNodeLike {

    def argDryRun(): Unit
  }
}

trait TypeOfMixin extends HasReflection {

  import reflection._

  class TypeOf[T](
      //  TODO: this API won't work for multiple types (e.g. showing common join & meet in heyting algebra)
      //   it should be replaced by a more general construct
      val tt: universe.Type
  ) {

    type TT = T

    lazy val typeView: TypeView = reflection.typeView(tt)

    lazy val selfGroup: VisualisationGroup = VisualisationGroup()
    lazy val nodes: selfGroup.Nodes = selfGroup.Nodes(typeView.formattedBy(format.typeFormat))
    lazy val showHierarchy: format.GraphFormat.Group#Viz[VisualisationGroup.Node] =
      Local.AnyGraph
        .Outbound(nodes.SuperTypeNode)
        .diagram_linkedHierarchy(selfGroup.delegateGroup)

    lazy val typeStr: String = nodes.typeText

    override def toString: String = {

      showHierarchy.toString
    }

    def should_=:=(that: TypeOf[_] = null): Unit = {

      val Seq(s1, s2) = Seq(this, that).map { v =>
        Option(v).map(_.showHierarchy.toString)
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

    implicit def asNodes(v: TypeOf[_]): v.selfGroup.Nodes = v.nodes
  }

  object VisualisationGroup extends Local.AnyGraph.Outbound.UntypedDef {

    // technically this layer could be collapsed into GraphRepr
    trait Node extends UntypedNode with TypeOfMixin.VNodeLike {}
  }

  // visualisations in the same group should not display redundant information
  case class VisualisationGroup() {

    import VisualisationGroup._

    lazy val delegateGroup: format.GraphFormat.Group = format.GraphFormat.Group()

    case class Nodes(
        ir: TypeIR,
        visited: mutable.ArrayBuffer[TypeID] = mutable.ArrayBuffer.empty
    ) {

      val node: TypeView = ir.typeView

      {
        visited += node.reference
      }

      lazy val argGraph = {

        val equivalentIRs = ir.EquivalentTypes.recursively

        val argNodes = equivalentIRs
          .groupBy(_.typeView)
          .flatMap {
            case (_, vs) =>
              val argNode = this.copy(ir = vs.head).ArgNode
              if (argNode.induction.isEmpty) None
              else Some(argNode)
          }
          .toSeq

        Local.AnyGraph.Outbound(argNodes: _*)
      }

      lazy val typeText: String = ir.text

      lazy val argViz: delegateGroup.Viz[VisualisationGroup.Node] = delegateGroup.Viz(argGraph)

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

      case object SuperTypeNode extends Node {

        final override lazy val identityKeyC = Some(node.reference)

        override protected val inductionC = {
          val existing = visited.distinct

          node.superTypes.flatMap { tt =>
            if (visited.contains(tt.reference)) None
            else {
              Some(Nodes(tt.formattedBy(format.typeFormat), visited).SuperTypeNode)
            }
          }
        }

        override lazy val toString: String = fullText

        override def argDryRun(): Unit = {

          argViz.dryRun()
        }
      }

      case object ArgNode extends Node {

        final override lazy val identityKeyC = None

        override protected lazy val inductionC = {
          node.args.map { tt =>
            Nodes(tt.formattedBy(format.typeFormat)).SuperTypeNode
          }
        }

        override lazy val toString: String = {

          val ttStr = node.self.typeConstructor.toString

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