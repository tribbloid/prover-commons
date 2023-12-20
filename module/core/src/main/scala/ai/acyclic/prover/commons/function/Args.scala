package ai.acyclic.prover.commons.function

import shapeless.{::, HList, HNil}
import shapeless.ops.hlist.Tupler
import shapeless.ops.record.Selector
import shapeless.tag.@@

import scala.language.{dynamics, implicitConversions}

case class Args[+H <: HList](repr: H) extends ArgsLike with Args.ArgsDynamics[H] {

  final def asTuple[HH >: H <: HList, TT](
      implicit
      tupler: Tupler.Aux[HH, TT]
  ): TT = tupler(repr)

  final def asHList: H = repr

  final def unbox1[T](
      implicit
      is: H <:< (T :: HNil)
  ): T = repr.head
}

object Args {
  // dynamic API, safety delegated to macro
  trait ArgsDynamics[+H <: HList] extends Dynamic {
    self: Args[H] =>

    def selectDynamic[HH >: H <: HList](key: String)(
        implicit
        selector: Selector[HH, Symbol @@ key.type]
    ): selector.Out = selector(repr)
  }

  implicit def unbox1Directly[T](v: Args[T :: HNil]): T = v.unbox1

  implicit def asTuple[H <: HList, TT](v: Args[H])(
      implicit
      tupler: Tupler.Aux[H, TT]
  ): TT = v.asTuple
}
