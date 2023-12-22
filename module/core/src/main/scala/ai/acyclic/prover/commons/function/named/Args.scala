package ai.acyclic.prover.commons.function.named

import ai.acyclic.prover.commons.util.ArgsLike
import shapeless.ops.hlist.Tupler
import shapeless.ops.record.Selector
import shapeless.tag.@@
import shapeless.{::, HList, HNil}

import scala.language.{dynamics, implicitConversions}

/**
  * mimicking named tuple proposal of Scala3
  * @param asHList
  *   shapeless representation in runtime
  * @tparam H
  *   ... and compile-time
  */
case class Args[+H <: HList](asHList: H) extends ArgsLike with Args.Dynamics[H] {

  final def asTuple[HH >: H <: HList, TT](
      implicit
      tupler: Tupler.Aux[HH, TT]
  ): TT = tupler(asHList)

  final def unbox[T](
      implicit
      is: H <:< (T :: HNil)
  ): T = asHList.head
}

object Args {

  type NoArg = Args[HNil] // phantom type, do not instantiate it

  // dynamic API, safety delegated to macro
  sealed trait Dynamics[+H <: HList] extends Dynamic {
    self: Args[H] =>

    def selectDynamic[HH >: H <: HList](key: String)(
        implicit
        ev: Selector[HH, Symbol @@ key.type]
    ): ev.Out = ev(asHList)
  }

  implicit def unbox1Directly[T](v: Args[T :: HNil]): T = v.unbox

  implicit def asTuple[H <: HList, TT](v: Args[H])(
      implicit
      ev: Tupler.Aux[H, TT]
  ): TT = v.asTuple
}
