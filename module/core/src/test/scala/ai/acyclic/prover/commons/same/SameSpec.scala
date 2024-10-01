package ai.acyclic.prover.commons.same

import ai.acyclic.prover.commons.testlib.BaseSpec

class SameSpec extends BaseSpec {

  import Same._
  import SameSpec._

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

  it(ByEquals.getClass.getSimpleName) {
    val set = fixtures.map { v =>
      ByEquals.Wrapper(v)
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

  it(ByProductElements.getClass.getSimpleName) {
    val set = fixtures.map { v =>
      ByProductElements.Wrapper(v)
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

  it(classOf[ByConstruction].getSimpleName) {
    val set = fixtures.map { v =>
      ByToleranceExample.Wrapper(v)
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

      val lookup = ByEquals.Lookup[Int, String]()

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

  object ByToleranceExample extends Same.ByConstruction {

    override def truncateToTolerance(v: Any): Option[Any] = {
      v match {
        case d: Double => Some(d.toInt.toDouble)
        case _         => None
      }
    }
  }

}
