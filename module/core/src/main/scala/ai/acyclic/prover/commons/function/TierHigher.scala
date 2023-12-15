package ai.acyclic.prover.commons.function

import shapeless.HNil

trait TierHigher extends Tier {
  self: Singleton =>

  import shapeless.::

  val lower: Tier
  type IUB = ::[Any, lower.IUB]

  implicit class FnXOps[I <: IUB, +R](self: Function[I, R]) {

    case class curry[IH, IT <: lower.IUB](v: IH)(
        implicit
        ev: (IH :: IT) =:= I
    ) extends lower.Derived[IT, R](self)(
          { args =>
            val thisArgs = Args[I](ev(args.self.::(v)))

            self.hApply(thisArgs)
          }
        )
  }

  implicit class PureXOps[I <: IUB, +R](self: Pure[I, R]) {

    case class memoize(args: Args[I])
        extends T0.Derived[HNil, R](self)(
          { _ =>
            self.hApply(args)
          }
        )
        with T0.Cached[HNil, R]
  }
}
