package ai.acyclic.prover.commons.jit.bound

import ai.acyclic.prover.commons.util.Phantom

abstract class Bound extends Phantom() {

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
  type Max
  type Min <: Max

//  type T >: Min <: Max

  type Bound = Bound.Lt[this.Min, this.Max]

  type Less = Bound.Lt[this.Min, this.Max]
}

object Bound {

  type Lt[-_Min <: _Max, +_Max] = Bound {
    type Min >: _Min
    type Max <: _Max
  }
  type <~>[_Min, _Max >: _Min] = Lt[_Min, _Max]

  abstract class Impl[_Min, _Max >: _Min] extends Bound {
    final type Min = _Min
    final type Max = _Max
  }

  type Gt[_Min, _Max >: _Min] = Bound {
    type Min <: _Min
    type Max >: _Max
  }
  type >~<[_Min, _Max >: _Min] = Gt[_Min, _Max]
  abstract class Gt_[_Min, _Max >: _Min] extends Bound {
    type Min <: _Min
    type Max >: _Max
  }

  type Top = Lt[Nothing, Any]
  val Top: Top = Phantom()

  type Point[T] = Lt[T, T]
  def Point[T]: Point[T] = Phantom()

  type |[T] = Point[T]
}
