package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.refl.Reflection
import ai.acyclic.prover.commons.viz.format.Formats0

import scala.language.implicitConversions

trait TypeViz[R <: Reflection] extends TermAndTypeOfMixin {

  override val reflection: R

  type TTag[T] <: WeakTypeTag[T]

  def of(tt: Type) = new TypeOf[Any](tt)

  def apply[T](
      implicit
      ev: TTag[T]
  ): TypeOf[T] = {
    new TypeOf[T](ev.tpe)
  }

  def infer[T](v: T)(
      implicit
      ev: TTag[T]
  ): TermAndTypeOf[T] = new TermAndTypeOf(v, ev.tpe)

  def narrow[T](v: T)(
      implicit
      ev: TTag[v.type]
  ): TermAndTypeOf[v.type] = new TermAndTypeOf(v, ev.tpe)
}

object TypeViz {

  def default[R <: Reflection](reflection: R) =
    new TypeVizBuilder[R](reflection, TypeTreeFormat.Default).WeakType

  trait TestFixtures { // TODO: move to test

    val TypeViz = TypeViz.this

    val TypeVizShort = {

      val format = Formats0.TypeInfo.HidePackage.recursively.DeAlias
      TypeViz.withFormat(format)
    }

    val TypeVizDeAlias = {

      val format = Formats0.TypeInfo.DeAlias
      TypeViz.withFormat(format)
    }
  }

  implicit def asRuntimeDefault(self: TypeViz.type): TypeVizBuilder.RuntimeDefault.WeakType.type =
    TypeVizBuilder.RuntimeDefault.WeakType
}
