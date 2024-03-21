package ai.acyclic.prover.commons.same

import ai.acyclic.prover.commons.collection.CacheView.{MapRepr, SetRepr}
import ai.acyclic.prover.commons.collection.{CacheView, KeyEncodedMap, MapBackedSet, ValueEncodedMap}
import ai.acyclic.prover.commons.function.Bijection
import ai.acyclic.prover.commons.util.Caching

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.reflect.ClassTag

trait Same[T] {

  def sameness: Same.By
}

object Same {

  protected def rectify(id: Any): Any = {
    val result = id match {
      case aa: Array[_] => aa.toList
      case _            => id
    }
    result
  }

  object ConstructionID {

    protected object Impl extends (Any => Int) {
      override def apply(v1: Any): Int = System.identityHashCode(v1)
    }

    def apply[T]: T => Int = Impl.asInstanceOf[T => Int]

  }

  trait By extends Serializable {

    protected def _getID(v1: Any): Option[Any]

    final def getID(v1: Any): Option[Any] = {
      _getID(rectify(v1))
    }

    protected def proveNonTrivial(v1: Any, v2: Any): Boolean = {

      (getID(v1), getID(v2)) match {

        case (Some(id1), Some(id2)) => id1 == id2
        case _                      => false
      }
    }

    /*
     must satisfy:
     - reflexivity: if (v1 eq v2), then areSame(v1, v2)
     - symmetry: if areSame(v1, v2) then areSame(v2, v2)
     */
    final def check(v1: Any, v2: Any): Boolean = {

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

    final def checkNot(v1: Any, v2: Any): Boolean = !check(v1, v2)

    final def checkSafely(v1: Any, v2: Any): Boolean = {

      val result = check(v1, v2)
      val swapped = check(v2, v1)
      require(result == swapped, "sameness should be symmetric")
      if (result)
        require(
          getID(v1).get == getID(v2).get,
          s"""
             |if two objects are the same, their IDs should be identical, however:
             |Left  ID: ${getID(v1)}
             |Right ID: ${getID(v2)}
             |""".stripMargin.trim
        )
      result
    }

    trait ^[T] extends Same[T] {
      final override def sameness: By = By.this
    }

    def idOrConstructionHash(v: Any): Int = {

      getID(v) match {
        case Some(id) =>
          id.##
        case None =>
          ConstructionID.apply(this)
      }
    }

    trait IWrapper {
      protected def samenessDelegatedTo: Any

      final override def hashCode(): Int = {
        idOrConstructionHash(samenessDelegatedTo)
      }

      final override def equals(that: Any): Boolean = {
        that match {
          case that: IWrapper => By.this.checkSafely(samenessDelegatedTo, that.samenessDelegatedTo)
          case _              => false
        }
      }
    }

    case class Wrapper[T](override val samenessDelegatedTo: T) extends IWrapper {

      override def toString: String = "" + samenessDelegatedTo
    }

    // a cache wrapper with a serialID, such that `values` will return the values in insertion order
    case class Lookup[K, V](
        underlying: CacheView[Wrapper[K], (V, Long)] = Caching.Soft.build[Wrapper[K], (V, Long)]()
    ) extends CacheView[K, V] {

      private val serialID: AtomicInteger = new AtomicInteger(0)
      @volatile var locked: Boolean = false

      def nextSerialID: Int = {
        require(
          !locked, {
            locked
            "cannot write, lookup is locked"
          }
        )
        serialID.getAndIncrement()
      }

      @transient lazy val asMap: MapRepr[K, V] = {

        val keyCodec = new Bijection[K, Wrapper[K]] {
          override def apply(v1: K): Wrapper[K] = Wrapper(v1)

          override def invert(v1: Wrapper[K]): K = v1.samenessDelegatedTo
        }

        val valueCodec = new Bijection[V, (V, Long)] {
          override def apply(v1: V): (V, Long) = (v1, nextSerialID)

          override def invert(v1: (V, Long)): V = v1._1
        }

        val keyEnc = new KeyEncodedMap.Mutable(
          keyCodec,
          underlying
        )

        new ValueEncodedMap.Mutable(
          valueCodec,
          keyEnc
        ) {

          override def values: Seq[V] = underlying.values.toSeq.sortBy(_._2).map(_._1)
        }
      }

      override def asSet(
          implicit
          ev: Unit <:< V
      ): SetRepr[K] = {

        new MapBackedSet.Mutable(asMap.asInstanceOf[mutable.Map[K, Unit]])
        // alas, Scala compiler is too dumb, should figure it out automatically
      }

    }

    case class Of[T: ClassTag](fn: T => Option[Any]) extends By {

      def outer: By.this.type = By.this

      override protected def _getID(v1: Any): Option[Any] = {
        v1 match {
          case vv: T =>
            outer.getID(fn(vv))
          case _ =>
            None
        }
      }

      override protected def proveNonTrivial(v1: Any, v2: Any): Boolean = {

        (v1, v2) match {
          case (vv1: T, vv2: T) =>
            (fn(vv1), fn(vv2)) match {
              case (Some(t1), Some(t2)) =>
                outer.proveNonTrivial(t1, t2)
              case _ => false
            }

          case _ => false
        }
      }
    }
  }

  object ByConstruction extends By {

    override def _getID(v1: Any): Some[Any] = Some(ConstructionID[Any](v1))

    override def proveNonTrivial(v1: Any, v2: Any): Boolean = {
      (v1, v2) match {
        case (_v1: AnyRef, _v2: AnyRef) => _v1.eq(_v2)
        case _                          => super.proveNonTrivial(v1, v2)
      }
    }
  }

  object ByEquality extends By {
    override def _getID(v1: Any): Some[Any] = Some(v1)
  }

  trait ByProductWithTolerance extends By {

    override def _getID(v1: Any): Some[Any] = {

      val result = truncateToTolerance(v1) match {
        case Some(r) => r
        case None =>
          v1 match {
            case v: Product =>
              v.productPrefix -> v.productIterator.map(_getID).toList
            case _ =>
              v1
          }
      }
      Some(result)
    }

    def truncateToTolerance(v: Any): Option[Any]
  }

  object ByProduct extends ByProductWithTolerance {

    def truncateToTolerance(v: Any): Option[Any] = None
  }
}
