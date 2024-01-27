package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.function.Thunk
import ai.acyclic.prover.commons.util.{Caching, ConstructionID}

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

  trait By {

    protected def _getID(v1: Any): Option[Any]

    @transient final def getID(v1: Any): Option[Any] = {
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

    trait Facade {
      protected def samenessDelegatedTo: Any

      final override def hashCode(): Int = {
        idOrConstructionHash(samenessDelegatedTo)
      }

      final override def equals(that: Any): Boolean = {
        that match {
          case that: Facade => By.this.checkSafely(samenessDelegatedTo, that.samenessDelegatedTo)
          case _            => false
        }
      }
    }

    case class Wrapper[T](override val samenessDelegatedTo: T) extends Facade {

      final override def toString: String = "" + samenessDelegatedTo
    }

    case class Correspondence[K, V]() {

      lazy val lookup: Caching.Soft.ConcurrentCache[Wrapper[K], Thunk[V]] =
        Caching.Soft.ConcurrentCache[Wrapper[K], Thunk[V]]()

      // TODO: how to remove it? it is slow
      lazy val collection: mutable.Buffer[Wrapper[K]] = mutable.Buffer.empty

      def values: Seq[V] = collection.map(k => lookup(k).value).toSeq

      final def getOrElseUpdate(key: K, getValue: => V): V = {

        val id = Wrapper(key)
        val w =
          this.synchronized {
            lookup.getOrElseUpdate(
              id, {
                val t = Thunk(_ => getValue)
                collection += id
                t
              }
            ) // should be fast
          }
        w.value
        // TODO: this may cause stackoverflow
        //  if [[getOrElseUpdate]] is called again within the thunk
        //  may need a trampoline to avoid it, maybe switch to cats
      }

      final def updateOverride(key: K, getValue: => V): Unit = {

        val id = Wrapper(key)
        this.synchronized {
          lookup.update(
            id, {
              val t = Thunk(_ => getValue)
              collection += id
              t
            }
          )
        }
      }

      final def remove(key: K): Unit = {
        val id = Wrapper(key)
        this.synchronized {
          lookup.remove(id)
          collection.remove(
            collection.indexOf(id)
          )
        }
      }



      final def isEmpty: Boolean = lookup.isEmpty

      final def get(key: K): Option[V] = {
        val inMemoryId = Wrapper(key)

        lookup
          .get(inMemoryId)
          .map(_.value)
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
