package ai.acyclic.prover.commons.multiverse

import ai.acyclic.prover.commons.collection.LookupMagnet.{MapRepr, SetRepr}
import ai.acyclic.prover.commons.collection.{KeyEncodedMap, LookupMagnet, MapBackedSet, ValueEncodedMap}
import ai.acyclic.prover.commons.function.Bijection
import ai.acyclic.prover.commons.util.Caching

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * this is an extension of multiversal equality (a.k.a. equalverse) proposal
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
object CanEqual {

  def hashOnStack(v1: Any): Int = {

    v1 match {
      case _: AnyRef => System.identityHashCode(v1)
      case x         => x.hashCode()
    }
  }

  case class Projection[T](
      canEqual: CanEqual[T],
      value: T
  ) extends View.Equals.Base {

    override lazy val canEqualProjections = Vector(this)

    def internalHashCode(): Int = {
      ???
    }

    def internalEquals(that: Projection[?]): Boolean = {
      ???
    }
  }

  abstract class Impl[LR](
      implicit
      val tagLR: ClassTag[LR]
  ) extends CanEqual[LR] {

    object Validate extends CanEqual.Impl[LR] {

      val outer = Impl.this

      override def hashOfNonTrivial(v: LR): Option[Int] = outer.hashOfNonTrivial(v)

      override def areEqualNonTrivial(x: LR, y: LR): Option[Boolean] = {

        // TODO: how to validate transitivity?

        val fwd = outer.areEqualNonTrivial(x, y)
        val rev = outer.areEqualNonTrivial(y, x)

        val refl1 = outer.areEqual(x, x)
        val refl2 = outer.areEqual(y, y)

        require(refl1, s"validation failed: not reflexive on ${x}")
        require(refl2, s"validation failed: not reflexive on ${y}")

        require(fwd == rev, "validation failed: not symmetric")

        if (fwd.contains(true))
          require(
            hashOf(x) == hashOf(y),
            s"""
               |if two objects are the same, their hash values should be identical, however:
               |Left  Hash: ${hashOf(x)}
               |Right Hash: ${hashOf(y)}
               |""".stripMargin.trim
          )
        fwd
      }
    }
  }

  object Native extends Impl[Any] {
    // be very careful of loop elimination
    // if a class override hash and equality method and delegate to Same, it will cause infinite loop

    override def hashOfNonTrivial(v: Any): Option[Int] = v match {
      case v: NonTerminating =>
        throw NoTerminationException(
          s"cannot use `${v}`: ${v.getClass.getCanonicalName} in multiverse hashing, will cause infinite loop"
        )
      case v =>
        Some(v.##)
    }

    override def areEqualNonTrivial(v1: Any, v2: Any): Option[Boolean] = {

      (v1, v2) match {
        case (v: NonTerminating, _) =>
          throw NoTerminationException(
            s"cannot use `${v}`: left side ${v.getClass.getCanonicalName} in multiverse equality, will cause infinite loop"
          )
        case (_, v: NonTerminating) =>
          throw NoTerminationException(
            s"cannot use `${v}`: right side ${v.getClass.getCanonicalName} in multiverse equality, will cause infinite loop"
          )
        case (ll, rr) => Some(ll == rr)
      }
    }
  }

  object ByStackMemory extends Impl[Any] {
    // always delegate to trivial case

    override def hashOf(v: Any): Int = {
      hashOnStack(v)
    }

    override def hashOfNonTrivial(v: Any): Option[Int] = {
      Some(hashOf(v))
    }

    override def areEqual(v1: Any, v2: Any): Boolean = {
      hashOnStack(v1) == hashOnStack(v2)
    }

    override def areEqualNonTrivial(v1: Any, v2: Any): Option[Boolean] = {
      Some(hashOnStack(v1) == hashOnStack(v2))
    }
  }

  case class Quotient[T: ClassTag](
      left: CanEqual[T],
      right: CanEqual[T]
  ) extends CanEqual.Impl[T] {
    // TODO: finish this later
    // fase / false = unknown
    // true / false = error
    // true /true = true
    // false / true = false

    override def hashOfNonTrivial(v: T): Option[Int] = ???

    override def areEqualNonTrivial(v1: T, v2: T): Option[Boolean] = ???
  }

  case class OrElse[T: ClassTag](
      primary: CanEqual[T],
      fallback: CanEqual[T]
  ) extends CanEqual.Impl[T] {

    override def hashOfNonTrivial(v: T): Option[Int] = {
      primary
        .hashOfNonTrivial(v)
        .orElse(
          fallback.hashOfNonTrivial(v)
        )
    }

    override def areEqualNonTrivial(v1: T, v2: T): Option[Boolean] = {

      primary
        .areEqualNonTrivial(v1, v2)
        .orElse(
          fallback.areEqualNonTrivial(v1, v2)
        )
    }
  }

  case class DirectSum[T: ClassTag](
      left: CanEqual[T],
      right: CanEqual[T]
  ) extends CanEqual.Impl[T]() {
    // TODO: finish this later
    // false + false = false
    // true + false = false
    // false + true = false
    // true + true = true

    override def hashOfNonTrivial(v: T): Option[Int] = ???

    override def areEqualNonTrivial(v1: T, v2: T): Option[Boolean] = ???
  }

  case class ByNormalise[LR: ClassTag](
      normalise: CanNormalise[LR] = CanNormalise.Native,
      fallback: CanEqual[Any] = CanEqual.Native
  ) extends CanEqual.Impl[LR] {

    override def hashOfNonTrivial(v: LR): Option[Int] = {
      val normalFormOpt = normalise.normalise(v)

      normalFormOpt
        .flatMap { n =>
          fallback.hashOfNonTrivial(n)
        }
    }

    override def areEqualNonTrivial(v1: LR, v2: LR): Option[Boolean] = {

      val opts: Seq[Option[Any]] = Seq(v1, v2).map { v =>
        normalise.normalise(v)
      }

      val Seq(lOpt, rOpt) = opts

      (lOpt, rOpt) match {
        case (Some(ln), Some(rn)) =>
          fallback
            .areEqualNonTrivial(ln, rn)
            .orElse(
              fallback.areEqualNonTrivial(v1, v2)
            )

        case _ =>
          fallback.areEqualNonTrivial(v1, v2)
      }
    }
  }

  case class ByUnapply[LR](
      canUnapply: CanUnapply[LR] = CanUnapply.Native,
      fallback: CanEqual[Any] = CanEqual.Native
  )(
      implicit
      override val tagLR: ClassTag[LR]
  ) extends CanEqual.Impl[LR] {

    final override def hashOfNonTrivial(v: LR): Option[Int] = {

      canUnapply
        .unapply(v)
        .flatMap { p =>
          val vs = p.elements

          val subs: Vector[Option[Int]] = vs.map { v =>
            this.ForAny
              .hashOfNonTrivial(v)
              .orElse(
                fallback.hashOfNonTrivial(v)
              )
          }

          val result: Option[Int] =
            if (!subs.contains(None)) Some(subs.map(_.get).##)
            else None

          result
        }
        .orElse {
          fallback.hashOfNonTrivial(v)
        }
    }

    final override def areEqualNonTrivial(v1: LR, v2: LR): Option[Boolean] = {

      val opts: Seq[Option[UnappliedForm]] = Seq(v1, v2).map { v =>
        canUnapply.unapply(v)
      }

      val Seq(ll, rr) = opts

      (ll, rr) match {
        case (Some(lkvs), Some(rkvs)) =>
          if (lkvs.prefix != rkvs.prefix) return Some(false);
          if (lkvs.schema != rkvs.schema) return Some(false);

          val subs: Vector[Option[Boolean]] = lkvs.elements
            .zip(rkvs.elements)
            .map {
              case (lv, rv) =>
                this.ForAny
                  .areEqualNonTrivial(lv, rv)
                  .orElse(
                    fallback.areEqualNonTrivial(lv, rv)
                  )
            }

          val result: Option[Boolean] =
            if (!subs.contains(None)) Some(subs.map(_.get).forall(identity))
            else None

          result
        case _ =>
          fallback.areEqualNonTrivial(v1, v2)
      }
    }
  }

  implicit class Tagged[LR: ClassTag](self: CanEqual[LR]) {

    object ForAny extends Impl[Any] {

      val outer = CanEqual.this

      override def hashOfNonTrivial(v: Any): Option[Int] = {

        v match {
          case vv: LR => self.hashOfNonTrivial(vv)
          case _      => None
        }
      }

      override def areEqualNonTrivial(v1: Any, v2: Any): Option[Boolean] = {

        (v1, v2) match {
          case (null, null) => Some(true)
          case (null, _)    => Some(false)
          case (_, null)    => Some(false)
          case (ll: LR, rr: LR) =>
            self.areEqualNonTrivial(ll, rr)
          case _ => None
        }
      }

    }

    def <~[T](value: T): Projection[T] = CanEqual.Projection(this.ForAny, value)

    def apply[T] = <~[T] _
  }
}

trait CanEqual[-LR] extends Plane {

  import CanEqual.*

  trait NonTerminating extends AnyRef

  def hashOfNonTrivial(v: LR): Option[Int]
  // TODO: should return Int directly
  // TODO: what about long hash?
  // subclasses should strive to make equal instances to have identical hashcode, but is it possible?

  def hashOf(v: LR): Int = {

    hashOfNonTrivial(v).getOrElse {
      ByStackMemory.hashOfNonTrivial(v).get
    }
  }

  def areEqualNonTrivial(v1: LR, v2: LR): Option[Boolean]

  def areEqual(x: LR, y: LR): Boolean = {
    val determined: Option[Boolean] = (x, y) match {
      case (x: AnyRef with LR, y: AnyRef with LR) => {
        if (x.eq(y)) Some(true)
        else areEqualNonTrivial(x, y)
      }
      case (_: AnyRef with LR, _) => Some(false)
      case (_, _: AnyRef with LR) => Some(false)
      case _ =>
        areEqualNonTrivial(x, y)
    }

    determined.getOrElse(
      ByStackMemory.areEqualNonTrivial(x, y).get
    )
  }

  final def notEqual(x: LR, y: LR): Boolean = !areEqual(x, y)

  case object Lookup extends Serializable

  // a cache wrapper with a serialID, such that `values` will return the values in insertion order
  case class Lookup[K <: LR, V](
      underlying: LookupMagnet[Projection[K], (V, Long)] = Caching.Soft.build[Projection[K], (V, Long)]()
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

      val keyCodec = new Bijection[K, Projection[V]] {
        override def apply(v1: K): Projection[V] = <~(v1)

        override def invert(v1: Projection[V]): K = v1.value
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
