package org.shapesafe.graph.commons.util.viz

trait TypeVizLike extends TermAndTypeOfs {

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

object TypeVizLike {}
