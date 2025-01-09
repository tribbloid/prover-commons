package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.diff.StringDiff
import ai.acyclic.prover.commons.graph.Arrow
import ai.acyclic.prover.commons.graph.local.Local
import ai.acyclic.prover.commons.graph.viz.{Flow, Hierarchy, LinkedHierarchy, RefGroup}
import ai.acyclic.prover.commons.refl.HasReflection
import ai.acyclic.prover.commons.typesetting.{Padding, TextBlock}

import scala.language.implicitConversions

object TypeOfMixin {

  trait VNodeLike {

//    lazy val scanArgsOnce: Unit = {}
  }
}

trait TypeOfMixin extends HasReflection {

  import reflection.*

  val treeFormat: TypeTreeFormat

  // visualisations in the same group should not display redundant information
  case class InRefGroup(refGroup: RefGroup) extends Local.Diverging.UpperSemilattice.Codomain {

    object backbone_sameRefGroup extends LinkedHierarchy {

      override val backbone: Hierarchy = treeFormat.backbone.backbone
      override val getRefGroup: () => RefGroup = () => refGroup

      override def maxRecursionDepth: Int = treeFormat.backbone.maxRecursionDepth
    }

    trait node extends Node_ with TypeOfMixin.VNodeLike {}

    case class NodeBuilder(
        ir: TypeIR
    ) {

      def base: TypeOps = ir.typeView

      lazy val typeText: String = ir.text

      case object SuperTypes {

        case object Node extends node {

          {
            if (treeFormat._argsBeforeSubtypes) {
              // nodeText contains args info, it will be eagerly initialised if the treeFormat said so
              nodeText
            }
          }

          final override lazy val identityC: Some[TypeOfMixin.this.reflection.TypeID] = Some(base.reference)

          override val inductions: List[(Arrow.`~>`, node)] = {

            base.superTypes_nonTransitive
              .filter { tv =>
                tv.toString != "Any" // skipped for being too trivial
              }
              .map { tv =>
                NodeBuilder(tv.formattedBy(treeFormat.typeFormat)).SuperTypes.Node
              }
          }

          private lazy val args = Args.effective

          override lazy val nodeText: String = {

            args
              .map { args =>
                val argsText = args.graph.text_linkedHierarchy(backbone_sameRefGroup).toString

                val indented =
                  TextBlock(argsText).pad
                    .left(Padding.argLeftBracket)
                    .indent("      ")
                    .build

                typeText + "\n" + indented
              }
              .getOrElse(typeText)
          }
        }
      }

      object Args {

        case object Node extends node {

          final override lazy val identityC: None.type = None

          override lazy val inductions: List[(Arrow.`~>`, node)] = {
            base.args.map { tt =>
              NodeBuilder(tt.formattedBy(treeFormat.typeFormat)).SuperTypes.Node
            }
          }

          override lazy val nodeText: String = {

            val ttStr = base.as.typeConstructor.toString

            val size = base.args.size

            if (size == 1) s"$ttStr [ $size ARG ] :"
            else if (size == 0) s"$ttStr [ No ARG ]"
            else s"$ttStr [ $size ARGS ] :"
          }
        }

        lazy val allNodes: Seq[node] = {

          val equivalentIRs = ir.EquivalentTypes.recursively

          val results = equivalentIRs
            .groupBy(_.typeView)
            .flatMap {
              case (_, vs) =>
                val argNode = NodeBuilder.this.copy(ir = vs.head).Args.Node
                if (argNode.inductions.isEmpty) None
                else Some(argNode)
            }
            .toSeq

          results
        }

        lazy val effective: Option[Args.type] = {
          if (allNodes.isEmpty) None
          else Some(this)
        }

        lazy val graph: Local.Diverging.Graph.Graph[node] = {

          Local(allNodes*)
        }

      }

      lazy val text_linkedHierarchy: LinkedHierarchy#IVisual = {

        SuperTypes.Node.text_linkedHierarchy(backbone_sameRefGroup)
      }

      lazy val text_flow: Flow#IVisual = {

        refGroup.convertNode(SuperTypes.Node).text_flow()
      }
    }

//    case class Visual(
//        ir: TypeIR
//    ) {
//
//      lazy val builder: NodeBuilder = {
//        NodeBuilder(ir.formattedBy(treeFormat.typeFormat))
//      }
//
//      lazy val typeStr: String = builder.typeText
//
//      lazy val entry: builder.SuperTypes.Node.type = builder.SuperTypes.Node
//    }
  }

  object TypeOf {

    implicit def asIR(v: TypeOf[?]): TypeIR = v.ir
  }

  class TypeOf[T](
      //  TODO: this API won't work for multiple types (e.g. showing common join & meet in heyting algebra)
      //   it should be replaced by a more general construct
      val tt: universe.Type
  ) {

    private lazy val ops: TypeOps = reflection.typeView(tt)

    private lazy val ir = TypeIR(ops, treeFormat.typeFormat)

    def text_linkedHierarchy(
        refGroup: RefGroup = treeFormat.backbone.getRefGroup()
    ): LinkedHierarchy#IVisual = {

      val inRefGroup = InRefGroup(refGroup)
      inRefGroup.NodeBuilder(ir).text_linkedHierarchy
    }

    def text_flow(
        refGroup: RefGroup = treeFormat.backbone.getRefGroup()
    ): Flow#IVisual = {

      val inRefGroup = InRefGroup(refGroup)
      inRefGroup.NodeBuilder(ir).text_flow
    }

//    object InGroup extends InGroup(treeFormat.backbone.getRefGroup())

//    case class InGroup(
//        refGroup: RefGroup = treeFormat.backbone.getRefGroup()
//    ) {
//
////      lazy val graph: Local.Diverging.Graph[VNodeDomain.node] = {
////
////        /**
////          * CAUTION: [[irView.SuperTypeNode]] is a semilattice node in Heyting algebra.
////          *
////          * BUT if counting reference it becomes a diverging graph, potentially with cycle.
////          *
////          * which is why it returned a [[Local.Diverging.Graph]] constructed from
////          * [[Local.Diverging.UpperSemilattice.Node]]
////          */
////        Local.Diverging.Graph.makeExact(primary)
////      }
//    }

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

}
