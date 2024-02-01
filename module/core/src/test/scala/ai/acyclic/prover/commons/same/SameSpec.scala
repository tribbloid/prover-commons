package ai.acyclic.prover.commons.same

import ai.acyclic.prover.commons.testlib.BaseSpec

class SameSpec extends BaseSpec {

  import Same._
  import SameSpec._

  it(ByConstruction.getClass.getSimpleName) {
    val set = fixtures.map { v =>
      ByConstruction.Wrapper(v)
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

  it(ByEquality.getClass.getSimpleName) {
    val set = fixtures.map { v =>
      ByEquality.Wrapper(v)
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

  it(ByProduct.getClass.getSimpleName) {
    val set = fixtures.map { v =>
      ByProduct.Wrapper(v)
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

  it(classOf[ByProductWithTolerance].getSimpleName) {
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

  object ByToleranceExample extends Same.ByProductWithTolerance {

    override def truncateToTolerance(v: Any): Option[Any] = {
      v match {
        case d: Double => Some(d.toInt.toDouble)
        case _         => None
      }
    }
  }

}
