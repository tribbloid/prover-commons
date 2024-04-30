package ai.acyclic.prover.commons.cap

import scala.language.implicitConversions

object Pending {

//  trait Must[-S]

  // DO NOT CHANGE! will be delegated to kyo in Scala 3
  type <<[+T, -S] >: T // | Must[S]
  // S is contravariant because Pending can be added implicitly

  private def __sanity(): Unit = {

    trait Ex

    trait IO1 {}

    trait IO2 {}

//      implicitly[(Ex << IO1) <:< (Ex << IO1 << IO2)] only works in Scala 3
    implicitly[(Ex << IO1) <:< (Ex << IO2 << IO1)]
    implicitly[(Ex << IO1) <:< (Ex << (IO1 with IO2))]
  }

  sealed trait Add[C] {

    def apply[V](v: V): V << C = v.asInstanceOf[V << C]
  }

  sealed trait Revoke[C] {

    def apply[V](v: V << C): V = v.asInstanceOf[V]
  }

  case class Annotator[C]() {

    object add extends Add[C]

    // TODO: sometimes left associated function won't work (generic collapse to Nothing), need to file a bug report for it
    def <<: : add.type = add

    object revoke extends Revoke[C]

    def <<--: : revoke.type = revoke
  }

  trait OrNull

  object OrNull extends Annotator[OrNull] {

    case class Ops[T](self: T << OrNull) {

      def asOption: Option[T] = self match {
        case null => None
        case v    => Some(v.asInstanceOf[T])
      }
    }

    implicit def ops[T](self: T << OrNull): Ops[T] = Ops(self)

    //    implicit def ops[T <: Object](self: T << OrNull): Ops[T] = Ops(self) only works in Scala 3

    //    implicit def ops2(self: String << OrNull): Ops[String] = Ops(self) works, but not general enough
  }

  trait OrThrow[E <: Throwable]

  object OrThrow {

    case class Ops[T, E <: Throwable](self: T << OrThrow[E]) {

      def asEither: Either[E, T] = {
        self match {
          case fn: (() => T) =>
            try {
              val v = self.asInstanceOf[() => T].apply()
              Right(v)
            } catch {
              case e: E => Left(e)
            }
          case v: T =>
            Right(v)
        }
      }
    }
  }

}
