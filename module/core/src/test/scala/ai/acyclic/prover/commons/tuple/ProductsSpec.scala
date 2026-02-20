package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec

class ProductsSpec extends BaseSpec {

  import Fixture.SrcSystem.*

  describe("Products.Zippable") {

    it("can zip and unzip empty with empty") {
      val zippable = implicitly[Zippable.Aux[Eye, Eye, Eye]]

      val a: Eye = Eye
      val b: Eye = Eye

      val zipped = zippable.zip(a, b)
      assert(zipped == Eye)

      val unzipped = zippable.unzip(zipped)
      assert(unzipped == (Eye, Eye))
    }

    it("can zip and unzip empty with non-empty") {
      val zippable = implicitly[Zippable.Aux[Eye, Int ><: Eye, Int ><: Eye]]

      val a: Eye = Eye
      val b: Int ><: Eye = 1 ><: Eye

      val zipped = zippable.zip(a, b)
      assert(zipped == b)

      val unzipped = zippable.unzip(zipped)
      assert(unzipped == (Eye, b))
    }

    it("can zip and unzip non-empty with empty") {
      val zippable = implicitly[Zippable.Aux[Int ><: Eye, Eye, Int ><: Eye]]

      val a: Int ><: Eye = 1 ><: Eye
      val b: Eye = Eye

      val zipped = zippable.zip(a, b)
      assert(zipped == 1 ><: Eye)

      val unzipped = zippable.unzip(zipped)
      assert(unzipped == (a, Eye))
    }

    it("can zip and unzip non-empty with non-empty") {
      val zippable = implicitly[
        Zippable.Aux[Int ><: String ><: Eye, Boolean ><: Double ><: Eye, Int ><: String ><: Boolean ><: Double ><: Eye]
      ]

      val a: Int ><: String ><: Eye = 1 ><: "a" ><: Eye
      val b: Boolean ><: Double ><: Eye = true ><: 2.0 ><: Eye

      val zipped = zippable.zip(a, b)
      assert(zipped == 1 ><: "a" ><: true ><: 2.0 ><: Eye)

      val unzipped = zippable.unzip(zipped)
      assert(unzipped == (a, b))
    }
  }
}
