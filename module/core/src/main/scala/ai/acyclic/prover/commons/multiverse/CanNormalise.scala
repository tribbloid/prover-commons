package ai.acyclic.prover.commons.multiverse

import ai.acyclic.prover.commons.util.Magnet.OptionMagnet

import scala.reflect.ClassTag

object CanNormalise {

  def rectifyArray(v: Any): Any = {
    val result = v match {
      case aa: Array[_] => aa.toList
      case _            => v
    }
    result
  }

  object Native extends CanNormalise[Any] {

    // JVM doesn't have a native normalisation rule
    override def normalise(v: Any): OptionMagnet[NormalForm] = None
  }

  implicit class Fn[T: ClassTag](
      fn: T => OptionMagnet[NormalForm]
  ) extends CanNormalise[T] {

    override def normalise(v: T): OptionMagnet[NormalForm] = fn(v)
  }

  implicit class FnDirect[T: ClassTag](
      fn: T => OptionMagnet[Any]
  ) extends Fn[T]({ v: T =>
        val result = fn(v).map(NormalForm(_))
        result
      })

  implicit class Tagged[T: ClassTag](self: CanNormalise[T]) {

    object ForAny extends CanNormalise[Any] {

      val outer = CanNormalise.this

      override def normalise(v: Any): OptionMagnet[NormalForm] = {
        rectifyArray(v) match {
          case v: T => self.normalise(v)
          case _    => None
        }
      }
    }
  }
}

trait CanNormalise[-T] extends Plane {

  trait NonTerminating extends AnyRef

  def normalise(v: T): OptionMagnet[NormalForm]
  // normalised form is assumed to be always equal `v`, validation may be required to be impl later
}
