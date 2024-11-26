package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.util.Erased

trait TypeBound extends Erased {

  /**
    * used like a type argument with lower & upper bound
    *
    * e.g. [[fn[T >: Min <: Max](t: T): Unit]] is equivalent to:
    *
    * [[fn[B <: TypeBound.K[Min, Max](implicit bound: B = Erased())(t: bound.Range): Unit]]
    *
    * OR (with type projection)
    *
    * [[fn[B <: TypeBound.K[Min, Max](t: B#Range): Unit]]
    *
    * but it has the extra benefit of making Min & Max accessible, which is not possible in before
    */

  type Min <: Max
  type Max

//  type T >: Min <: Max

  type Lt = TypeBound.Lt[this.Min, this.Max]
  type Gt = TypeBound.Gt[this.Min, this.Max]
}

object TypeBound {
  type K[TMin, TMax >: TMin] = TypeBound {
    type Min = TMin
    type Max = TMax
  }
  type |~|[TMin, TMax >: TMin] = K[TMin, TMax]
  trait K_[TMin, TMax >: TMin] extends TypeBound {
    type Min = TMin
    type Max = TMax
  }

  type Lt[TMin, TMax >: TMin] = TypeBound {
    type Min >: TMin
    type Max <: TMax
  }
  type <~>[TMin, TMax >: TMin] = Lt[TMin, TMax]
  trait Lt_[TMin, TMax >: TMin] extends TypeBound {
    type Min >: TMin
    type Max <: TMax
  }

  type Gt[TMin, TMax >: TMin] = TypeBound {
    type Min <: TMin
    type Max >: TMax
  }
  type >~<[TMin, TMax >: TMin] = Gt[TMin, TMax]
  trait Gt_[TMin, TMax >: TMin] extends TypeBound {
    type Min <: TMin
    type Max >: TMax
  }

  type Top = K[Nothing, Any]
  val Top: Top = Erased()

  trait PinpointAt[T] extends K_[T, T] {
    type Point = Max
  }
  type |[T] = K[T, T]

  type Pinpoint = PinpointAt[?]
}
