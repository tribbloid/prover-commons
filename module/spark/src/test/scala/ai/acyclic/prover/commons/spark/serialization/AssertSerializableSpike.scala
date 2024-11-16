package ai.acyclic.prover.commons.spark.serialization

import ai.acyclic.prover.commons.function.hom.Hom
import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.util.Caching

import scala.util.Try

class AssertSerializableSpike extends BaseSpec {
  import AssertSerializableSpike.*

  describe("should be WeaklySerializable - ")
  /** EndMarker */
  {

    classOfIt {

      val trial = Try {
        require(
          requirement = false,
          "error!"
        )
      }
      val ee = trial.failed.get
      ee
    } { v =>
      AssertSerializable(v).weakly()

      //    TestHelper.TestSC.parallelize(Seq(ee))
      //      .collect() //TODO: this failed, why?
    }

    describe("by ClosureCleaner") {

      it("0") {

        AssertSerializable(Outer.inner0).weakly()
      }

      it("1") {

        AssertSerializable(Outer.inner1).weakly()
      }

      it("2") {

        AssertSerializable(Outer.inner2).weakly()
      }

      it("3") {

        AssertSerializable(Outer.inner3).weakly()
      }
    }
  }

  classOfIt {

    val trial = Try {
      require(
        requirement = false,
        "error!"
      )
    }
    val ee = trial.failed.get
    ee
  } { v =>
    AssertSerializable(v).weakly()

    //    TestHelper.TestSC.parallelize(Seq(ee))
    //      .collect() //TODO: this failed, why?
  }

  describe("by ClosureCleaner") {

    it("0") {

      AssertSerializable(Outer.inner0).weakly()
    }

    it("1") {

      AssertSerializable(Outer.inner1).weakly()
    }

    it("2") {

      AssertSerializable(Outer.inner2).weakly()
    }

    it("3") {

      AssertSerializable(Outer.inner3).weakly()
    }
  }

  describe("should be Serializable with equality - ") {

    typeOfIt {
      (): Unit
    } { v =>
      AssertSerializable(v).weakly()
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

case object AssertSerializableSpike {

  trait Fn[-I, +O] extends (I => O) with Serializable {}

  object Outer extends NOTSerializable {

    // everything here should be extracted safely by Spark Closure cleaner

    val inner0: String => Int = { (_: String) =>
      3
    }

    val inner1: Fn[String, Int] = new Fn[String, Int] {
      override def apply(v1: String): Int = 3
    }

    val inner2: Hom.Impl.Circuit[String, Int] = Hom.Circuit { _ =>
      3
    }

    val inner3: String => Int = inner0
  }
}
