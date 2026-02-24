package ai.acyclic.prover.spark.serialization

import ai.acyclic.prover.commons.jit.eval.Args
import Args.{><:, T0}
import ai.acyclic.prover.commons.jit.Hom.:|~>
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

        val circuit: ai.acyclic.prover.commons.jit.Hom.Fn[String ><: T0, Int] =
          ai.acyclic.prover.commons.jit.Hom.Fn.at[String] { _ =>
            3
          }

        val poly: ai.acyclic.prover.commons.jit.Hom.Poly = new ai.acyclic.prover.commons.jit.Hom.Poly {}

        val unnaturalTransformation: Seq :|~> Vector =
          new ai.acyclic.prover.commons.jit.Hom.Impl.UnnaturalTransformation[Seq, Vector] {

            override def refine[T <: Any]: ai.acyclic.prover.commons.jit.Hom.Fn[Seq[T] ><: T0, Vector[T]] =
              ai.acyclic.prover.commons.jit.Hom.Fn.at[Seq[T]](v => v.toVector)
          }

//        val dependent: Hom.Dependent[Vector] = new Hom.Impl.Dependent[Vector] {
//
//          override def apply[T <: Any](arg: T): Vector[T] = Vector(arg)
//        }
      }

      import Outer.*

      Seq(
        //      singleAbstractMethod,
        Seq(circuit, circuit.cached()),
        poly,
        Seq(unnaturalTransformation, unnaturalTransformation.cached())
//        Seq(dependent, dependent.cached())
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
      AssertSerializable[Caching.Strong._Cache[String, Int]](v).on { (v1, v2) =>
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
      val v = Caching.Weak._Cache[String, Int]()
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
