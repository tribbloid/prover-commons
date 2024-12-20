package ai.acyclic.prover.commons.multiverse

import ai.acyclic.prover.commons.multiverse.CanUnapply.Quotient

import scala.reflect.ClassTag

object CanUnapply {

  object Bypass extends CanUnapply[Any] {

    override def unapply(v: Any): Option[UnappliedForm] = None
  }

  object Native extends CanUnapply[scala.Product] {

    override def unapply(v: Product): Option[UnappliedForm] = {

      val schema = v.productElementNames.toVector
      val values = v.productIterator.toVector

      Some(new UnappliedForm.Named(schema, values, v.productPrefix))
    }
  }

  case class Quotient[T: ClassTag](
      left: CanUnapply[T],
      right: CanUnapply[T]
  ) extends CanUnapply[T] {

    override def unapply(v: T): Option[UnappliedForm] = {

      val (ll, rr) = left.unapply(v) -> right.unapply(v)

      val _rr = rr.toSet

      val keysToBeRemoved = _rr.flatMap { v: UnappliedForm =>
        v.schema.flatten
      }

//      val valuesToBeRemoved = _rr.flatMap { v =>
//        v.values
//      }

      ll.map { v =>
        v.remove(keys = keysToBeRemoved)
      }
    }
  }

  case class DirectSum[T: ClassTag](
      left: CanUnapply[T],
      right: CanUnapply[T]
  ) extends CanUnapply[T] {

    override def unapply(v: T): Option[UnappliedForm] = {

      val (ll, rr) = left.unapply(v) -> right.unapply(v)
      ??? // TODO: finish this later
    }
  }

  implicit class Tagged[T: ClassTag](self: CanUnapply[T]) {

    object ForAny extends CanUnapply[Any] {

      def outer = CanUnapply.this

      override def unapply(v: Any): Option[UnappliedForm] = {
        v match {
          case v: T => self.unapply(v)
          case _    => None
        }
      }
    }
  }
}

trait CanUnapply[-T] extends Plane {

  def unapply(v: T): Option[UnappliedForm]

  case class AndThen[T2](that: UnappliedForm => UnappliedForm) extends CanUnapply[T] {

    override def unapply(v: T): Option[UnappliedForm] = {

      CanUnapply.this.unapply(v).map(that)
    }
  }

  def quotient[T2 <: T: ClassTag](that: CanUnapply[T2]): Quotient[T2] = {

    Quotient(this, that)
  }

  def /[T2 <: T: ClassTag](that: CanUnapply[T2]): Quotient[T2] = quotient[T2](that)
}
