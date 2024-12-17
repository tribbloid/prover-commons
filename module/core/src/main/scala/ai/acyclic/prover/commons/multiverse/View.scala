package ai.acyclic.prover.commons.multiverse

import scala.collection.mutable.ArrayBuffer

trait View {}

object View {

  type Equals = Equals.Base

  object Equals {

    // what works needs to be done here?
    trait Base extends View with scala.Equals with CanEqual.Native.NonTerminating {

      final val canEqualProjections: ArrayBuffer[CanEqual.Projection[?]] = ArrayBuffer.empty

      final override def hashCode(): Int = {

        canEqualProjections
          .map { p =>
            p.internalHashCode()
          }
          .hashCode()
      }

      final override def canEqual(that: Any): Boolean = {
        that match {
          case _that: Base => this.canEqualProjections.size == _that.canEqualProjections.size
          case _           => false
        }
      }

      final override def equals(that: Any): Boolean = {
        if (canEqual(that)) {

          this.canEqualProjections
            .zip(that.asInstanceOf[Base].canEqualProjections)
            .forall {
              case (ll, rr) =>
                ll.internalEquals(rr)
            }

        } else false
      }
    }

    trait ByConstruction extends Base {

      final protected val constructionID: java.util.UUID = java.util.UUID.randomUUID()
    }

    object ByConstruction {

      val canEqual: CanEqual.ByUnapply[Product] = {

        val fallback = CanEqual.ByNormalise[ByConstruction] { v =>
          Some(v.constructionID)
        }

        CanEqual.ByUnapply(
          CanUnapply.Native,
          fallback.ForAny
        )
      }
    }
  }

  // TODO: finish it later
//  object Product {
//
//    trait Proto[T] extends View with scala.Product with CanUnapply.Native.NonTerminating {
//
//      @transient lazy val unapplyForm = unapply(unapplyBy).get
//
//      @transient override lazy val productIterator: Iterator[Any] = {
//        ???
//      }
//
////      def tagT: ClassTag[T]
////      def projection: T
////      def : CanEqual[T]
//    }
//  }

}
