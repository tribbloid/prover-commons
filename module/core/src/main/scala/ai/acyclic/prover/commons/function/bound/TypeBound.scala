package ai.acyclic.prover.commons.function.bound

import ai.acyclic.prover.commons.util.Erased

trait TypeBound extends PseudoTypeBound {

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
  type Max >: Min

//  type T >: Min <: Max

  type Bound = TypeBound.K[this.Min, this.Max]

  type Less = TypeBound.Lt[this.Min, this.Max]
  type More = TypeBound.Gt[this.Min, this.Max]
}

object TypeBound {
  type K[TMin, TMax >: TMin] = TypeBound {
    type Min = TMin
    type Max = TMax
  }
  type |~|[TMin, TMax >: TMin] = K[TMin, TMax]
  trait K_[TMin, TMax >: TMin] extends TypeBound {
    final type Min = TMin
    final type Max = TMax
  }

  type Lt[TMin, TMax >: TMin] = TypeBound {
    type Min >: TMin
    type Max <: TMax
  }
  type <~>[TMin, TMax >: TMin] = Lt[TMin, TMax]
//  trait Lt_[TMin, TMax >: TMin] extends TypeBound {
//    type Min >: TMin
//    type Max >: Min <: TMax
//  }

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
  trait Top_ extends TypeBound {
    type Min = Nothing
    type Max = Any
  }
  val Top: Top = Erased()

  trait Point extends TypeBound {
    type Point
    type Max = Point
    type Min = Point
  }
  type PointAt[T] = Point {
    type Point = T
  }
  type |[T] = PointAt[T]

}
