package ai.acyclic.prover.commons.multiverse

import scala.reflect.{classTag, ClassTag}

object CanUnapply {

  object Native extends CanUnapply[scala.Product] {

    override def unapply(v: Product): Option[UnappliedForm] = {

      val schema = v.productElementNames.toVector
      val values = v.productIterator.toVector

      Some(new UnappliedForm.Named(v.productPrefix, schema, values))
    }
  }

  case class Quotient[T: ClassTag](
      left: CanUnapply[T],
      right: CanUnapply[T]
  ) extends CanUnapply[T] {

    override def unapply(v: T): Option[UnappliedForm] = {

      val (ll, rr) = left.unapply(v) -> right.unapply(v)

      val toBeRemoved = rr.toSet.flatMap { v: UnappliedForm =>
        v.schema.flatten
      }

      ll.map { v =>
        v.removeKeys(toBeRemoved)
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

  trait NonTerminating extends AnyRef

  def unapply(v: T): Option[UnappliedForm]
}
