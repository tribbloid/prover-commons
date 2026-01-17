package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition

trait Expr[
    +O
] extends Traceable.RuntimeAOT[O] {
  // this makes it a subtype of Tracer[T, O] where T can be anything, in which case it represents the argument T being discarded

  type Pending

  // type IsConcrete // reserved for tracking if an expression can be evaluated immediately at compile-time

  def getConcrete(
      implicit
      defAt: SrcDefinition
  ): O = throw new ConcretizationTypeError(this, defAt)

  def higherOrder(
      implicit
      defAt: SrcDefinition
  ): TracingFn.Static[Unit, O] = {

    TracingFn.Impl(
      ???
    )
  }
}

object Expr {

  implicit def _getValue[T](v: Gt[?, T])(
      implicit
      position: SrcDefinition = null
  ): T =
    v.getConcrete

  infix type Aux[P, +O] = Expr[O] { type Pending = P }
// infix type Lt[+P, +O] = Expr[O] { type Pending <: P }
  infix type Gt[-P, +O] = Expr[O] { type Pending >: P }

  /**
    * The pending input becomes irrelevant, thus can accept anything
    */
  type Discarding[+O] = Gt[Any, O]

  {
    implicitly[(Int => String) <:< (Int => Any)]
    implicitly[(Any => Int) <:< (String => Int)]

    /**
      * [[Gt]] is contravariant:
      *   - [[Gt]][NeedGeneric, Int] works whenever Expr[NeedSpecific, Int] is required
      *   - [[Gt]][Any, Int] represents a static Int and works whenever Expr[?, Int] is required
      */
    implicitly[Gt[Any, Int] <:< Gt[String, Int]]
    implicitly[Discarding[Int] <:< Gt[String, Int]]
    implicitly[Discarding[Int] <:< Input[Int]]
  }

  /**
    * No pending input, getConcrete can compute immediately, can be smoothly converted into [[Discarding]]
    */
  type MayBeConcrete[+O] = Discarding[O] // <- this should be a subtype of Discarding

  trait Concrete[+O] extends Expr[O] {
    final type Pending = Any

    def concrete: O

    override def getConcrete(
        implicit
        defAt: SrcDefinition
    ): O = concrete

  }

  /**
    * Concrete, but every getConcrete always get the same value
    */
  trait Static[+O] extends Concrete[O] {

    val concrete: O
  }

//  case class Thunk[+T](value: Hom.Thunk[T]) extends Expr[Any, T] {
//    override def getValue(
//        implicit
//        position: SrcDefinition
//    ): T = value.apply()
//  }

  case class Tuple2[+O1, +O2](_1: Expr[O1], _2: Expr[O2]) extends Expr[(O1, O2)] {
    final type Pending = (_1.Pending, _2.Pending)

    override def getConcrete(
        implicit
        defAt: SrcDefinition
    ): (O1, O2) = (_1.getConcrete, _2.getConcrete)
  }
}
