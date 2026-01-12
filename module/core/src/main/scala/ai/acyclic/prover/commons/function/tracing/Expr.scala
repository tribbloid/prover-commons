package ai.acyclic.prover.commons.function.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.function.hom.Hom

trait Expr[
    -P, // type of pending variables in JIT tracing
    +O
] extends Tracer[O] {
  // this makes it a subtype of Tracer[T, O] where T can be anything, in which case it represents the argument T being discarded

  def getValue(
      implicit
      position: SrcDefinition
  ): O = throw new ConcretizationTypeError(this, position)
}

object Expr {

  {

    implicitly[(Int => String) <:< (Int => Any)]
    implicitly[(Any => Int) <:< (String => Int)]

    /**
      * [[Expr#P]] is contravariant:
      *   - Expr[NeedGeneric, Int] works whenever Expr[NeedSpecific, Int] is required
      *   - Expr[Any, Int] represents a static Int and works whenever Expr[?, Int] is required
      */
    implicitly[Expr[Any, Int] <:< Expr[String, Int]]
  }

  implicit def _getValue[T](v: Expr[?, T])(
      implicit
      position: SrcDefinition = null
  ): T =
    v.getValue

  case class Const[+T](value: T) extends Expr[Any, T] {
    override def getValue(
        implicit
        position: SrcDefinition
    ): T = value
  }

//  case class Thunk[+T](value: Hom.Thunk[T]) extends Expr[Any, T] {
//    override def getValue(
//        implicit
//        position: SrcDefinition
//    ): T = value.apply()
//  }

  case class _1[-P, +O](
      primary: Hom.Fn[P, O]
  ) extends Expr[P, O] {

    override def getValue(
        implicit
        position: SrcDefinition
    ): O = ???
  }
}
