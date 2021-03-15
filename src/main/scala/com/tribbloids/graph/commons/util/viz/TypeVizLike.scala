package com.tribbloids.graph.commons.util.viz

trait TypeVizLike extends TermAndTypeOfs {

  type F[T] <: WeakTypeTag[T]

  def apply(tt: Type) = new TypeOf(tt)

  def apply[T](
      implicit
      ev: F[T]
  ): TypeOf = {
    apply(ev.tpe)
  }

  def infer[T](v: T)(
      implicit
      ev: F[T]
  ): TermAndTypeOf[T] = new TermAndTypeOf(v, ev.tpe)

  def narrow[T](v: T)(
      implicit
      ev: F[v.type]
  ): TermAndTypeOf[v.type] = new TermAndTypeOf(v, ev.tpe)
}

object TypeVizLike {}
