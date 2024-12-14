package ai.acyclic.prover.commons.same

import ai.acyclic.prover.commons.same.CanEqual.{ByConstruction, Native}
import ai.acyclic.prover.commons.testlib.BaseSpec

class SameSpec extends BaseSpec {

  import CanEqual.*
  import SameSpec.*

  it(ByMemory.getClass.getSimpleName) {
    val set = fixtures.map { v =>
      ByMemory.Wrapper(v)
    }.toSet

    set
      .mkString("\n")
      .shouldBe(
        """
        |2.0-5
        |2.0-5
        |2.1-5
        |2.0-5
        |""".stripMargin
      )
  }

  it(Native.getClass.getSimpleName) {
    val set = fixtures.map { v =>
      Native.Wrapper(v)
    }.toSet

    set
      .mkString("\n")
      .shouldBe(
        """
          |2.0-5
          |2.1-5
          |2.0-5
          |""".stripMargin
      )
  }

  it(ByConstruction.toString()) {
    val set = fixtures.map { v =>
      byConstructionExample.Wrapper(v)
    }.toSet

    set
      .mkString("\n")
      .shouldBe(
        """
          |2.0-5
          |2.1-5
          |""".stripMargin
      )
  }

  it(ByConstruction.toString() + "-withTolerance") {
    val set = fixtures.map { v =>
      withToleranceExample.Wrapper(v)
    }.toSet

    set
      .mkString("\n")
      .shouldBe(
        """
          |2.0-5
          |""".stripMargin
      )
  }

  describe("lookup.asMap") {
    it("can insert") {

      val lookup = Native.Lookup[Int, String]()

      val asMap = lookup.asMap

      asMap.update(1, "a")

      assert(lookup.values.size == 1)
      assert(lookup.get(1) == Some("a"))

    }
  }

}

object SameSpec {

  trait F {

    case class E(
        x: Double,
        y: Int
    ) {

      override def toString: String = productIterator.mkString("-")
    }
  }
  object F1 extends F
  object F2 extends F

  val fixtures = Seq(
    F1.E(2.0, 5),
    F1.E(2.0, 5),
    F1.E(2.1, 5),
    F2.E(2.0, 5)
  )

  val byConstructionExample = ByConstruction(Native)

  val withToleranceExample = byConstructionExample.copy(
    outer = Native.Rounding[Double] { d =>
      Some(d.toInt.toDouble)
    }
  )

}
