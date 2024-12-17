package ai.acyclic.prover.commons.multiverse

import ai.acyclic.prover.commons.testlib.BaseSpec

class CanEqualSpec extends BaseSpec {

  import CanEqualSpec.*
  import ai.acyclic.prover.commons.multiverse.CanEqual.*

  it(ByMemory.toString) {
    val set = fixtures.map { v =>
      ByMemory.on(v)
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

  it(Native.toString) {
    val set = fixtures.map { v =>
      Native.on(v)
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

  it(ByUnapply.toString) {
    val set = fixtures.map { v =>
      byUnapply.on(v)
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

  it(ByUnapplyAndNormalise.toString + "-withTolerance") {
//    val hashes = fixtures.map(v => ByUnapplyAndNormalise.on(v).hashCode())

    val set = fixtures.map { v =>
      ByUnapplyAndNormalise.on(v)
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

object CanEqualSpec {

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

  val byNormalise = CanEqual
    .ByNormalise[Any] {
      case d: Double => Some(d.toInt.toDouble)
      case v @ _     => Some(v)
    }
    .ForAny

  val byUnapply = CanEqual.ByUnapply(CanUnapply.Native)

  val ByUnapplyAndNormalise = CanEqual.ByUnapply(CanUnapply.Native, byNormalise)
}
