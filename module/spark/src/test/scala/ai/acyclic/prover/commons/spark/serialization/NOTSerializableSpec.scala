package ai.acyclic.prover.commons.spark.serialization

import ai.acyclic.prover.commons.spark.TestHelper
import ai.acyclic.prover.commons.testlib.BaseSpec
import org.apache.spark.SparkException

object NOTSerializableSpec {

  case class Thing(str: String)
  case class Thing2(str: String) extends NOTSerializable {

    //  @throws(classOf[IOException])
    //  private def writeObject(out: ObjectOutputStream): Unit = {
    //    this.str
    //    out.defaultWriteObject()
    //  }
  }

  class Super1(str: String) extends NOTSerializable
  case class Thing3(str: String) extends Super1(str)

  trait Super2 extends NOTSerializable
  case class Thing4(str: String) extends Super2
}

class NOTSerializableSpec extends BaseSpec {

  import NOTSerializableSpec.*

  it("base class is serializable")

  /** EndMarker */
  /** EndMarker */
  {
    val thing = Thing("abc")

    AssertSerializable(thing).strongly()

    val in = 1 to 2
    val out = TestHelper.TestSC
      .parallelize(in)
      .map { v =>
        thing.str + v
      }
      .collect()
      .toSeq

    assert(out == Seq("abc1", "abc2"))
  }

  SerializerEnv.Default.allSerializers.foreach { ser =>
    describe(s"when using ${ser.getClass.getSimpleName}") {
      it(s"mixin will trigger a runtime error") {
        val thing = Thing2("abc")

        intercept[Exception] {
          AssertSerializable(thing, Seq(ser)).weakly()
        }
      }

      it(s"subclass of a class that inherits mixin will trigger a runtime error") {
        val thing = Thing3("abc")

        intercept[Exception] {
          AssertSerializable(thing, Seq(ser)).weakly()
        }
      }

      it(s"subclass of a trait that inherits mixin will trigger a runtime error") {
        val thing = Thing3("abc")

        intercept[Exception] {
          AssertSerializable(thing, Seq(ser)).weakly()
        }
      }
    }
  }

  it("mxin will trigger a runtime error in closure cleaning") {
    val thing = Thing2("abc")

    val in = 1 to 2
    intercept[SparkException] {
      TestHelper.TestSC.parallelize(in).map { v =>
        thing.str + v
      }
    }
  }
}
