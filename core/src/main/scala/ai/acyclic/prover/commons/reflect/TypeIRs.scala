package ai.acyclic.prover.commons.reflect

import ai.acyclic.prover.commons.graph.{Arrow, Tree}
import ai.acyclic.prover.commons.reflect.format.{IROutput, TypeFormat}
import ai.acyclic.prover.commons.{Padding, TextBlock}

import scala.language.implicitConversions

/**
  * types of elements of a formatted type expression:
  *
  * SubExpr
  *   - Reference
  *   - GenArg
  *     - Arg
  *     - PrefixArg
  *     - ...
  */
trait TypeIRs extends HasUniverse {
  self: Reflection =>

  case class TypeIR(
      typeView: TypeView,
      format: TypeFormat
  ) extends Tree.Node {

    val refl: Reflection = self

    lazy val output: IROutput = format.resolve(self).apply(this)

    def text: String = output.text
    lazy val annotations: Seq[TypeIR] = {

      val result = output.annotations.collect {
        case v: TypeIR => v
      }

      result
    }

    def derivedFrom: Seq[TypeIR] = output.derivedFrom.collect {
      case v: TypeIR => v
    }

    object EquivalentTypes {

      lazy val directly: Seq[TypeIR] =
//        (annotations ++ derivedFrom)
        derivedFrom
          .filter { v =>
            v.typeView.self =:= TypeIR.this.typeView.self
          }

      lazy val recursively: Seq[TypeIR] = {

        directly ++ directly.flatMap(v => v.EquivalentTypes.recursively)
      }
    }

//    lazy val canonical: TypeIR = derivedFromOpt match {
//      case Some(v) => v.canonical
//      case _ => this
//    }

//    lazy val parts: Seq[TypeIR] = directAnnotations ++ derivedFromOpt.toSeq.flatMap(v => v.parts)
//
//    lazy val altForms: Seq[TypeIR] = {
//
//      val args = this.args.map(_.self).toSet
//      val nonArgParts = parts.filter { v =>
//        !args.contains(v.self)
//      }
//      nonArgParts ++ derivedFromOpt
//    }
//    def forms: Seq[TypeIR] = Seq(this) ++ this.altForms.flatMap { ff =>
//      ff.forms
//    }

    override lazy val outbound = {

      val both: Seq[Tree.Node] = annotations ++
        Seq(
          GroupTag("(derivedFrom)", derivedFrom)
        )

      both.filter(v => !v.isLeaf)
    }

    override def nodeText: String = {

//      s"$text <<-[ $format ]-< ${typeView.toString}"

      val formatBlock = TextBlock(
        s"""
           |$format
           """.stripMargin
      )
        .padLeft(Padding.leftArrowUp)
        .build

      s"""
           |$text
           |$formatBlock
           |${typeView.toString}
           |""".stripMargin.trim
    }

    override def toString: String = nodeText

//    def withDelegate(v: TypeIR): Output = {
//      val sameFormat = v.format == this.format
//      val sameType = v.typeView == this.typeView
//
//      require(
//        !(sameFormat && sameType),
//        "cannot convert Formatting into Output: may trigger dead loop"
//      )
//      Output(
//        v.text,
//        derivedFrom = Option(v)
//      )
//    }
  }

  object TypeIR {

    implicit def asTypeView(v: TypeIR): TypeView = v.typeView
  }

  case class GroupTag(
      nodeText: String,
      override val outbound: Seq[Arrow.`~>`.Of[TypeIR]]
  ) extends Tree.Node {

    def isEmpty: Boolean = outbound.isEmpty
  }
}
