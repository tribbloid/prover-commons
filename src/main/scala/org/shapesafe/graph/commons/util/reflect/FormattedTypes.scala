package org.shapesafe.graph.commons.util.reflect

import org.shapesafe.graph.commons.util.TreeLike
import org.shapesafe.graph.commons.util.reflect.format.{Output, TypeFormat}

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
trait FormattedTypes extends HasUniverse {
  self: Reflection =>

  case class FormattedType(
      typeView: TypeView,
      format: TypeFormat
  ) extends TreeLike {

    val refl: Reflection = self

    lazy val output: Output = format.resolve(self).apply(this)

    def text: String = output.text
    protected def directParts: Seq[FormattedType] = {

      val result = output.parts.collect {
        case v: FormattedType => v
      }

      result.foreach { v =>
        require(text.contains(v.text), s"Reference '${v.text}' cannot be found in whole '$text'")
      }

      result
    }
//    protected def directGenArgs: Seq[FormattedType] = typeView.genArgs.map { v =>
//      FormattedType(v, format)
//    }

//    protected def directSubExprs: Seq[FormattedType] = directParts ++ directGenArgs

    def simplified: Option[FormattedType] = output.simplified.collect {
      case v: FormattedType => v
    }

    lazy val canonical: FormattedType = simplified match {
      case Some(v) => v.canonical
      case _ => this
    }

    lazy val parts: Seq[FormattedType] = directParts ++ simplified.toSeq.flatMap(v => v.parts)

    lazy val altForms: Seq[FormattedType] = {

      val args = this.args.map(_.self).toSet
      val nonArgParts = parts.filter { v =>
        !args.contains(v.self)
      }
      nonArgParts ++ simplified
    }

//    lazy val refsAndSimplifiedRecursively: Seq[FormattedType] = refsAndSimplified.flatMap { v =>
//      Seq(v) ++ v.refsAndSimplifiedRecursively
//    }

    override lazy val children: Seq[Group] = {

      Seq(
        Group("directParts", directParts),
        Group("simplified", simplified.toSeq)
      )
        .filter(v => !v.isEmpty)
    }

    def forms: Seq[FormattedType] = Seq(this) ++ this.altForms.flatMap { ff =>
      ff.forms
    }

    override def nodeString: String = s"$text - ${format}"

    override def toString: String = treeString

    def withCanonical(canonical: FormattedType): Output = {
      val sameFormat = canonical.format == this.format
      val sameType = canonical.typeView == this.typeView

      require(
        !(sameFormat && sameType),
        "cannot convert Formatting into Output: may trigger dead loop"
      )
      Output(
        canonical.text,
//        canonical.parts,
        simplified = Option(canonical)
      )
    }
  }

  object FormattedType {

    implicit def asTypeView(v: FormattedType): TypeView = v.typeView
  }

  case class Group(
      nodeString: String,
      override val children: Seq[FormattedType]
  ) extends TreeLike {

    def isEmpty: Boolean = children.isEmpty
  }
}
