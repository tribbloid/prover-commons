package ai.acyclic.prover.commons

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

trait Sameness[T] {

  def samenessEvidence: Sameness.Evidence
}

object Sameness {

  def rectify(id: Any): Any = {
    val result = id match {
      case aa: Array[_] => aa.toList
      case _            => id
    }
    result
  }

  trait Evidence {

    protected def getIDInternal(v1: Any): Any

    @transient final def getID(v1: Any): Any = {
      getIDInternal(rectify(v1))
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

    final def proveSafely(v1: Any, v2: Any): Boolean = {

      val result = prove(v1, v2)
      val swapped = prove(v2, v1)
      require(result == swapped)
      result
    }

    trait ForAll[T] extends Sameness[T] {
      final override def samenessEvidence: Evidence = Evidence.this
    }

    trait EqualByMixin {

      override def hashCode: Int = getID(this).##

      override def equals(that: Any): Boolean = {
        Evidence.this.prove(this, that)
      }
    }

    case class Wrapper[T](v: T) {

      final override def hashCode: Int = getID(v).##

      final override def equals(that: Any): Boolean = {
        that match {
          case Wrapper(thatV) => Evidence.this.prove(v, thatV)
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
          //      this.synchronized {
          lookup.getOrElseUpdate(
            inMemoryId, {
              val created = Thunk(getValue)
              collection += created
              created
            }
          ) // should be fast
        //      }
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

  object ByConstruction extends Evidence {

    override def getIDInternal(v1: Any): Any = util.constructionID[Any](v1)

    override def proveNonTrivial(v1: Any, v2: Any): Boolean = {
      (v1, v2) match {
        case (_v1: AnyRef, _v2: AnyRef) => _v1.eq(_v2)
        case _                          => getIDInternal(v1) == getIDInternal(v2)
      }
    }
  }

  object ByEquality extends Evidence {
    override def getIDInternal(v1: Any): Any = v1
  }

  trait ByFieldsWithTolerance extends Evidence {

    override def getIDInternal(v1: Any): Any = {

      v1 match {
        case v: Product =>
          v.productPrefix -> v.productIterator.map(truncateToTolerance).toList
        case _ =>
          v1
      }
    }

    def truncateToTolerance(v: Any): Any
  }

  object ByFields extends ByFieldsWithTolerance {

    def truncateToTolerance(v: Any): Any = v
  }
}
