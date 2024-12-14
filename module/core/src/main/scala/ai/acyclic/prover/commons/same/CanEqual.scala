package ai.acyclic.prover.commons.same

import ai.acyclic.prover.commons.collection.LookupMagnet.{MapRepr, SetRepr}
import ai.acyclic.prover.commons.collection.{KeyEncodedMap, LookupMagnet, MapBackedSet, ValueEncodedMap}
import ai.acyclic.prover.commons.function.Bijection
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
trait CanEqual extends Serializable {

  import ai.acyclic.prover.commons.same.CanEqual.*

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
        // TODO: should we support equality between different classes?
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

  case class Rounding[T: ClassTag](fn: T => Option[Any]) extends ByUnapply[T](CanEqual.this) {

    override protected def unapply(v1: T): Option[Vector[Any]] = {

      Some(Vector(fn(v1)))
    }
  }

  trait Equals {

    protected def samenessKey: Any

    final override def hashCode(): Int = {
      getHash(samenessKey)
    }

    final def canEqual(that: Any): Boolean = {
      that match {
        case _: Equals => true
        case _         => false
      }
    }

    final override def equals(that: Any): Boolean = {
      if (canEqual(that)) {
        prove_validate(samenessKey, that.asInstanceOf[Equals].samenessKey)
      } else false
    }
  }

  case class Wrapper[T](override val samenessKey: T) extends Equals {

    override def toString: String = "" + samenessKey
  }

  case object Lookup extends Serializable

  // a cache wrapper with a serialID, such that `values` will return the values in insertion order
  case class Lookup[K, V](
      underlying: LookupMagnet[Wrapper[K], (V, Long)] = Caching.Soft.build[Wrapper[K], (V, Long)]()
  ) extends LookupMagnet[K, V] {

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

        override def invert(v1: Wrapper[K]): K = v1.samenessKey
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

object CanEqual {

  object MemoryHash {

    protected object Only extends (Any => Int) {
      override def apply(v1: Any): Int = System.identityHashCode(v1)
    }

    def apply[T]: T => Int = Only // .asInstanceOf[T => Int]

  }

  object ByMemory extends CanEqual {
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

//  abstract class UnapplyMixin[T: ClassTag] extends Same {
//
//    protected def unapply(v1: T): Option[Any]
//
//    protected def unapplyAny(v: Any): Option[Any] = {
//
//      rectify(v) match {
//        case v: T => unapply(v)
//        case _    => None
//      }
//    }
//  }

  object Native extends CanEqual {

    override protected def getHashNonTrivial(v: Any): Option[Int] = Option(v).map(_.##)

    protected def proveNonTrivial(v1: Any, v2: Any): Boolean = {

      (Option(v1), Option(v2)) match {

        case (Some(id1), Some(id2)) => id1 == id2
        case _                      => false
      }
    }
  }

  abstract class ByNormalise[T: ClassTag, R] extends CanEqual {

    /*
    returns Some vector for elements used to create this object
    or None if the origin of this object cannot be determinedd
     */
    protected def unapply(v1: T): Option[R]

    protected def unapplyAny(v: Any): Option[R] = {

      rectify(v) match {
        case v: T => unapply(v)
        case _    => None
      }
    }
  }

  abstract class ByUnapply[T: ClassTag](outer: CanEqual) extends ByNormalise[T, Vector[Any]] {

    final override protected def getHashNonTrivial(v: Any): Option[Int] = {
      unapplyAny(v).map { vec =>
        val hashVec = vec.map(outer.getHash)

        hashVec.##
      }
    }

    final override protected def proveNonTrivial(v1: Any, v2: Any): Boolean = {

      (unapplyAny(v1), unapplyAny(v2)) match {

        case (Some(v1), Some(v2)) =>
          if (v1.size != v2.size) return false;

          v1.zip(v2)
            .forall {
              case (a, b) => outer.prove(a, b)
            }

        case _ => false
      }
    }
  }

  case class ByConstruction(outer: CanEqual) extends ByUnapply[Product](outer) {

    final override def unapply(v: Product): Some[Vector[Any]] = {

      val result = Vector(v.productPrefix) ++ v.productIterator
      Some(result)
    }
  }
}
