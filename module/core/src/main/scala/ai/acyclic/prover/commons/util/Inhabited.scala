package ai.acyclic.prover.commons.util

import scala.collection.immutable

// TODO: move to common
class Inhabited[+A](val default: A) extends MayExist[A]

object Inhabited extends Inhabited_Imp0 {
  implicit object DefaultDouble extends Inhabited[Double](0.0)
  implicit object DefaultFloat extends Inhabited[Float](0.0f)
  implicit object DefaultInt extends Inhabited[Int](0)
  implicit object DefaultLong extends Inhabited[Long](0L)
  implicit object DefaultShort extends Inhabited[Short](0)
  implicit object DefaultByte extends Inhabited[Byte](0)
  implicit object DefaultChar extends Inhabited[Char]('\u0000')
  implicit object DefaultBoolean extends Inhabited[Boolean](false)
  implicit object DefaultUnit extends Inhabited[Unit](())

  implicit def defaultSeq[A]: Inhabited[immutable.Seq[A]] = new Inhabited[immutable.Seq[A]](Nil)
  implicit def defaultSet[A]: Inhabited[Set[A]] = new Inhabited[Set[A]](Set())
  implicit def defaultMap[A, B]: Inhabited[Map[A, B]] = new Inhabited[Map[A, B]](Map[A, B]())
  implicit def defaultOption[A]: Inhabited[Option[A]] = new Inhabited[Option[A]](None)

  def value[A](
      implicit
      value: Inhabited[A]
  ): A = value.default
}
