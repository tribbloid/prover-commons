package ai.acyclic.prover.commons

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

trait Same[T] {

  def sameness: Same.Definition
}

object Same {

  protected def rectify(id: Any): Any = {
    val result = id match {
      case aa: Array[_] => aa.toList
      case _            => id
    }
    result
  }

  trait Definition {

    protected def _getID(v1: Any): Any

    @transient final def getID(v1: Any): Any = {
      _getID(rectify(v1))
    }

    protected def proveNonTrivial(v1: Any, v2: Any): Boolean = getID(v1) == getID(v2)

    /*
     must satisfy:
     - reflexivity: if (v1 eq v2), then areSame(v1, v2)
     - symmetry: if areSame(v1, v2) then areSame(v2, v2)
     */
    final def prove(v1: Any, v2: Any): Boolean = {

      (v1, v2) match {
        case (null, null) => true
        case (null, _)    => false
        case (_, null)    => false
        case (x: AnyRef, y: AnyRef) =>
          if (x.eq(y)) true
          else if (x.getClass == y.getClass && proveNonTrivial(x, y)) true
          else false
        case _ =>
          proveNonTrivial(v1, v2)
      }
    }

    final def refute(v1: Any, v2: Any): Boolean = !prove(v1, v2)

    final def proveSafely(v1: Any, v2: Any): Boolean = {

      val result = prove(v1, v2)
      val swapped = prove(v2, v1)
      require(result == swapped, "sameness should be symmetric")
      if (result) require(getID(v1) == getID(v2), "if two objects are the same, their IDs should be identical")
      result
    }

    trait ForAll[T] extends Same[T] {
      final override def sameness: Definition = Definition.this
    }

    trait EqualByMixin {

      override def hashCode: Int = getID(this).##

      override def equals(that: Any): Boolean = {
        Definition.this.proveSafely(this, that)
      }
    }

    case class Wrapper[T](v: T) {

      final override def toString: String = "" + v

      final override def hashCode: Int = getID(v).##

      final override def equals(that: Any): Boolean = {
        that match {
          case Wrapper(thatV) => Definition.this.proveSafely(v, thatV)
          case _              => false
        }
      }
    }

    case class Correspondence[K, V]() {

      case class Thunk(fn: () => V) {

        lazy val value: V = fn()
      }

      val lookup: TrieMap[Wrapper[K], Thunk] = TrieMap.empty
      val collection: mutable.Buffer[Thunk] = mutable.Buffer.empty

      def values: Seq[V] = collection.map(_.value).toSeq

      final def getOrElseUpdate(key: K, getValue: () => V): V = {

        val inMemoryId = Wrapper(key)
        val w =
          this.synchronized {
            lookup.getOrElseUpdate(
              inMemoryId, {
                val created = Thunk(getValue)
                collection += created
                created
              }
            ) // should be fast
          }
        w.value
        // TODO: this may cause stackoverflow
        //  if [[getOrElseUpdate]] is called again within the thunk
        //  may need a trampoline to avoid it, maybe switch to cats
      }

      final def get(key: K): Option[V] = {
        val inMemoryId = Wrapper(key)

        lookup
          .get(inMemoryId)
          .map(_.value)
      }
    }

    case class Memoize[K, V](fn: K => V) extends (K => V) with HasOuter {

      override lazy val outer = Correspondence[K, V]()

      final def apply(key: K): V = outer.getOrElseUpdate(key, () => fn(key))
    }
  }

  object ByConstruction extends Definition {

    override def _getID(v1: Any): Any = util.constructionID[Any](v1)

    override def proveNonTrivial(v1: Any, v2: Any): Boolean = {
      (v1, v2) match {
        case (_v1: AnyRef, _v2: AnyRef) => _v1.eq(_v2)
        case _                          => _getID(v1) == _getID(v2)
      }
    }
  }

  object ByEquality extends Definition {
    override def _getID(v1: Any): Any = v1
  }

  trait ByProductWithTolerance extends Definition {

    override def _getID(v1: Any): Any = {

      truncateToTolerance(v1) match {
        case Some(r) => r
        case None =>
          v1 match {
            case v: Product =>
              v.productPrefix -> v.productIterator.map(_getID).toList
            case _ =>
              v1
          }
      }
    }

    def truncateToTolerance(v: Any): Option[Any]
  }

  object ByProduct extends ByProductWithTolerance {

    def truncateToTolerance(v: Any): Option[Any] = None
  }
}
