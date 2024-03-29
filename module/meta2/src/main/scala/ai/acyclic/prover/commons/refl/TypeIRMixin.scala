package ai.acyclic.prover.commons.refl

import ai.acyclic.prover.commons.typesetting.{Padding, TextBlock}
import ai.acyclic.prover.commons.viz.TypeIROutput
import ai.acyclic.prover.commons.viz.format.TypeFormat

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
trait TypeIRMixin {
  self: Reflection =>

  trait IRLike {

    val children: Seq[IRLike]
  }

  case class TypeIR(
      typeOps: TypeOps,
      format: TypeFormat
  ) extends IRLike {

    val refl: Reflection = self

    lazy val output: TypeIROutput = format.resolve(self).apply(this)

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
            v.typeOps.unbox =:= TypeIR.this.typeOps.unbox
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

    lazy val children = {

      val both = annotations ++
        Seq(
          GroupTag("(derivedFrom)", derivedFrom)
        )

      both.filter(v => v.children.nonEmpty)
    }

    lazy val nodeText: String = {

      val formatBlock = TextBlock(
        s"""
           |$format
           """.stripMargin
      ).pad.left(Padding.leftArrowUp).build

      s"""
           |$text
           |$formatBlock
           |${typeOps.toString}
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

    implicit def asTypeView(v: TypeIR): TypeOps = v.typeOps
  }

  case class GroupTag(
      nodeText: String,
      children: Seq[TypeIR]
  ) extends IRLike {

    def isEmpty: Boolean = children.isEmpty
  }
}
