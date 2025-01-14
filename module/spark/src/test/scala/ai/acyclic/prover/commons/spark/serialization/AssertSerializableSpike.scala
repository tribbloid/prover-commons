package ai.acyclic.prover.commons.spark.serialization

import ai.acyclic.prover.commons.function.hom.Hom
import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.util.Caching

import scala.util.Try

class AssertSerializableSpike extends BaseSpec {

  import AssertSerializableSpike.*

  describe("weakly - ") {

    classOfIt {

      val attempts = Try {
        require(
          requirement = false,
          "error!"
        )
      }
      val ee = attempts.failed.get
      ee
    } { v =>
      AssertSerializable(v).weakly()

      //    TestHelper.TestSC.parallelize(Seq(ee))
      //      .collect() //TODO: this failed, why?
      v
    }

    describe("by ClosureCleaner - ") {

      object Outer extends NOTSerializable {

        // everything here should be extracted safely by Spark Closure cleaner

        val function0: String => Int = { (_: String) =>
          3
        }

        val function1: String => Int = function0
      }

      import Outer.*

      Seq(
        function0,
        function1
        //      poly.cached()
      ).zipWithIndex.foreach {
        case (vs: Seq[_], i) =>
          vs.foreach { v =>
            it(i.toString + ":" + v.getClass.getSimpleName) {
              AssertSerializable(v).weakly()
            }
          }

        case (v, i) =>
          it(i.toString + ":" + v.getClass.getSimpleName) {
            AssertSerializable(v).weakly()
          }
      }
    }
  }

  describe("strongly - ") {

    typeOfIt {
      (): Unit
    } { v =>
      AssertSerializable(v).strongly()
    }

    describe("by ClosureCleaner - ") {

      object Outer extends NOTSerializable {

        // everything here should be extracted safely by Spark Closure cleaner

        val singleAbstractMethod: Fn[String, Int] = (_: String) => 3 // TODO: cannot handle this

        val circuit: Hom.Fn[String, Int] = Hom.Fn { _ =>
          3
        }

        val poly: Hom.Poly = new Hom.Poly {}

        val poly1: Hom.Poly1[Seq, Vector] = new Hom.Impl.Poly1[Seq, Vector] {

          override def apply[T <: Any](arg: Seq[T]): Vector[T] = arg.toVector
        }

        val dependent: Hom.Dependent[Vector] = new Hom.Impl.Dependent[Vector] {

          override def apply[T <: Any](arg: T): Vector[T] = Vector(arg)
        }
      }

      import Outer.*

      Seq(
        //      singleAbstractMethod,
        Seq(circuit, circuit.cached()),
        poly,
        Seq(poly1, poly1.cached()),
        Seq(dependent, dependent.cached())
        //      poly.cached()
      ).zipWithIndex.foreach {
        case (vs: Seq[_], i) =>
          vs.foreach { v =>
            it(i.toString + ":" + v.getClass.getSimpleName) {
              AssertSerializable(v).strongly()
            }
          }

        case (v, i) =>
          it(i.toString + ":" + v.getClass.getSimpleName) {
            AssertSerializable(v).strongly()
          }
      }
    }
  }

  ignore("not working") {

    typeOfIt {
      val cache = Caching.Strong.underlyingBuilder.build[String, Int]()
      cache.put("A", 1)
      cache
    } { v =>
      AssertSerializable(v).weakly()
    }

    typeOfIt {
      val v = Caching.Strong.build[String, Int]()
      v.put("a", 1)
      v
    } { v =>
      AssertSerializable[Caching.Strong.Impl[String, Int]](v).on { (v1, v2) =>
        v1 == v2
      }
    }

    typeOfIt {
      val v = Caching.Soft.build[String, Int]()
      v.put("a", 1)
      v
    } { v =>
      AssertSerializable(v).weakly()

    }

    typeOfIt {
      val v = Caching.Weak.Impl[String, Int]()
      v.put("a", 1)
      v
    } { v =>
      AssertSerializable(v).weakly()
    }
  }

}

case object AssertSerializableSpike extends Serializable {

  trait Fn[-I, +O] extends (I => O) with Serializable {}

}
