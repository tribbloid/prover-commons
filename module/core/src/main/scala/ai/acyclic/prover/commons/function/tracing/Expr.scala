package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

trait Expr[
    +O
] extends Tracer[O] {
  // this makes it a subtype of Tracer[T, O] where T can be anything, in which case it represents the argument T being discarded

  type Pending

  def getValue(
      implicit
      position: SrcDefinition
  ): O = throw new ConcretizationTypeError(this, position)
}

object Expr {

  type Lt[+P, +O] = Expr[O] { type Pending <: P }
  type Gt[-P, +O] = Expr[O] { type Pending >: P }

  {

    implicitly[(Int => String) <:< (Int => Any)]
    implicitly[(Any => Int) <:< (String => Int)]

    /**
      * [[Expr#P]] is contravariant:
      *   - Expr[NeedGeneric, Int] works whenever Expr[NeedSpecific, Int] is required
      *   - Expr[Any, Int] represents a static Int and works whenever Expr[?, Int] is required
      */
    implicitly[Gt[Any, Int] <:< Gt[String, Int]]
  }

  implicit def _getValue[T](v: Gt[?, T])(
      implicit
      position: SrcDefinition = null
  ): T =
    v.getValue

//  case class Thunk[+T](value: Hom.Thunk[T]) extends Expr[Any, T] {
//    override def getValue(
//        implicit
//        position: SrcDefinition
//    ): T = value.apply()
//  }

  case class _1[P, +O]() extends Expr[O] {
    final type Pending = P

    override def getValue(
        implicit
        position: SrcDefinition
    ): O = ???
  }
}
