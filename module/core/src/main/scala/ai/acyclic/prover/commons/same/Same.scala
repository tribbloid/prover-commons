package ai.acyclic.prover.commons.same

import ai.acyclic.prover.commons.collection.CacheView.{MapRepr, SetRepr}
import ai.acyclic.prover.commons.collection.{CacheView, KeyEncodedMap, MapBackedSet, ValueEncodedMap}
import ai.acyclic.prover.commons.function.Bijection
import ai.acyclic.prover.commons.same.Same.{ByUnapply, MemoryHash}
import ai.acyclic.prover.commons.util.Caching

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.reflect.ClassTag

//

/**
  * this is an extension of multiversal equality proposal
  * (https://docs.scala-lang.org/scala3/reference/contextual/multiversal-equality.html)
  *
  * can handle identity, equality, isomorphism and equivalence
  *
  * for a given universe, any of these can be proven by:
  *   - memory hash/eq: very fast & always enabled
  *   - reflexivity: if B = A then A = B
  *   - associativeness: if A = B, B = C then A = C, B has to be summoned by tactic
  *   - reconstructability/info-preserving: if for all A & B, there is (A -> B, B -> A) that is an automorphism, then A
  *     \= B
  *   - construction: if A & B are in the same product type & all their construction elements are same in a chosen
  *     universe, then A = B
  *   - JVM native:
  *     - prove by construction if A & B are product or primitive types
  *     - prove by memory address in all other cases
  */
trait Same extends Serializable {

  protected def getHashNonTrivial(v: Any): Option[Int]

  final def getHash(v: Any): Int = {

    getHashNonTrivial(v) match {
      case Some(id) =>
        id.##
      case None =>
        MemoryHash.apply(this) // always fall back to MemoryHash
    }
  }

  // TODO: this should return Option[Boolean], there are many ways to prove equality
  protected def proveNonTrivial(v1: Any, v2: Any): Boolean

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

  final def prove_validate(v1: Any, v2: Any): Boolean = {

    val result = prove(v1, v2)
    val swapped = prove(v2, v1)
    require(result == swapped, "sameness should be symmetric")
    if (result)
      require(
        getHash(v1) == getHash(v2),
        s"""
           |if two objects are the same, their hash values should be identical, however:
           |Left  Hash: ${getHash(v1)}
           |Right Hash: ${getHash(v2)}
           |""".stripMargin.trim
      )
    result
  }

  case object ByConstruction extends ByUnapply[Product] {

    override def outer: Same.this.type = Same.this

    final override def unapply(v: Product): Some[Any] = {

      val result = v.productPrefix -> v.productIterator.toList
      Some(result)
    }

  }

  case class Truncate[T: ClassTag](fn: T => Option[Any]) extends ByUnapply[T] {

    override def outer: Same.this.type = Same.this

    override protected def unapply(v1: T): Option[Any] = {

      fn(v1)
    }
  }

  trait IWrapper {
    protected def self: Any

    final override def hashCode(): Int = {
      getHash(self)
    }

    final override def equals(that: Any): Boolean = {
      that match {
        case that: IWrapper => prove_validate(self, that.self)
        case _              => false
      }
    }
  }

  case class Wrapper[T](override val self: T) extends IWrapper {

    override def toString: String = "" + self
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

        override def invert(v1: Wrapper[K]): K = v1.self
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
}

object Same {

  object MemoryHash {

    protected object Impl extends (Any => Int) {
      override def apply(v1: Any): Int = System.identityHashCode(v1)
    }

    def apply[T]: T => Int = Impl.asInstanceOf[T => Int]

  }

  object ByMemory extends Same {
    // always delegate to trivial case

    override protected def getHashNonTrivial(v: Any): Option[Int] = None

    override protected def proveNonTrivial(v1: Any, v2: Any): Boolean = false
  }

  protected def rectify(id: Any): Any = {
    val result = id match {
      case aa: Array[_] => aa.toList
      case _            => id
    }
    result
  }

  abstract class UnapplyMixin[T: ClassTag] extends Same {

    protected def unapply(v1: T): Option[Any]

    protected def unapplyAny(v: Any): Option[Any] = {

      rectify(v) match {
        case v: T => unapply(v)
        case _    => None
      }
    }
  }

  object ByEquals extends UnapplyMixin[Any] {
    override def unapply(v1: Any): Some[Any] = Some(v1)

    override protected def getHashNonTrivial(v: Any): Option[Int] =
      unapplyAny(v).map(_.##)

    protected def proveNonTrivial(v1: Any, v2: Any): Boolean = {

      (unapplyAny(v1), unapplyAny(v2)) match {

        case (Some(id1), Some(id2)) => id1 == id2
        case _                      => false
      }
    }
  }

  abstract class ByUnapply[T: ClassTag] extends UnapplyMixin[T] {

    def outer: Same

    final override protected def getHashNonTrivial(v: Any): Option[Int] = {
      unapplyAny(v).map(outer.getHash)
    }

    final override protected def proveNonTrivial(v1: Any, v2: Any): Boolean = {

      (unapplyAny(v1), unapplyAny(v2)) match {

        case (Some(id1), Some(id2)) => outer.prove(id1, id2)
        case _                      => false
      }
    }
  }
}
