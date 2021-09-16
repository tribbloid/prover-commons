package org.shapesafe.graph.commons.util.reflect

import org.shapesafe.graph.commons.util.{Padding, TextBlock, TreeLike}
import org.shapesafe.graph.commons.util.reflect.format.{IROutput, TypeFormat}

import scala.language.implicitConversions

/**
  * types of elements of a formatted type expression:
  *
  * SubExpr
  * - Reference
  * - GenArg
  *   - Arg
  *   - PrefixArg
  *   - ...
  */
trait TypeIRs extends HasUniverse {
  self: Reflection =>

  case class TypeIR(
      typeView: TypeView,
      format: TypeFormat
  ) extends TreeLike {

    val refl: Reflection = self

    lazy val output: IROutput = format.resolve(self).apply(this)

    def text: String = output.text
    protected def directAnnotations: Seq[TypeIR] = {

      val result = output.annotations.collect {
        case v: TypeIR => v
      }

      result
    }
    lazy val annotations: Seq[TypeIR] = directAnnotations

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

    override lazy val children: Seq[TreeLike] = {

      annotations ++
        Seq(
          GroupTag("(derivedFrom)", derivedFrom)
        )
          .filter(v => !v.isEmpty)
    }

    override def nodeString: String = {

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
           |""".trim.stripMargin
    }

    override def toString: String = treeString

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
      nodeString: String,
      override val children: Seq[TypeIR]
  ) extends TreeLike {

    def isEmpty: Boolean = children.isEmpty
  }
}
