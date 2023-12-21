package ai.acyclic.prover.commons.util

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
case class NamedArgs[+H <: HList](asHList: H) extends ArgsLike with NamedArgs.Dynamics[H] {

  final def asTuple[HH >: H <: HList, TT](
      implicit
      tupler: Tupler.Aux[HH, TT]
  ): TT = tupler(asHList)

  final def value1[T](
      implicit
      is: H <:< (T :: HNil)
  ): T = asHList.head
}

object NamedArgs {

  // dynamic API, safety delegated to macro
  sealed trait Dynamics[+H <: HList] extends Dynamic {
    self: NamedArgs[H] =>

    def selectDynamic[HH >: H <: HList](key: String)(
        implicit
        ev: Selector[HH, Symbol @@ key.type]
    ): ev.Out = ev(asHList)
  }

  implicit def unbox1Directly[T](v: NamedArgs[T :: HNil]): T = v.value1

  implicit def asTuple[H <: HList, TT](v: NamedArgs[H])(
      implicit
      ev: Tupler.Aux[H, TT]
  ): TT = v.asTuple
}
