package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec

class ProductsSpec extends BaseSpec {

  import Fixture.SrcSystem.*

  object HListSystem extends Products.Monoidal {
    override type VBound = Any
    override type Element[T <: VBound] = T

    override type Prod = shapeless.HList
    override type Eye = shapeless.HNil
    override val Eye: Eye = shapeless.HNil
    override infix type ><:[L <: VBound, TAIL <: Prod] = shapeless.::[L, TAIL]

    override protected def cons[L <: VBound, TAIL <: Prod](head: Element[L], tail: TAIL): L ><: TAIL = head :: tail
    override def deCons[L <: VBound, TAIL <: Prod](cons: L ><: TAIL): (Element[L], TAIL) = cons.head -> cons.tail
  }

  describe("Products.Monoidal constructors") {

    it("of/ofNarrow should build Prod for HList-based monoidal systems") {
      import HListSystem.*

      val wide = of(1, "a")
      val narrow = ofNarrow(1, "a")

      val _: 1 ><: "a" ><: Eye = wide
      val _: 1 ><: "a" ><: Eye = narrow
      val _: Prod = wide
      val _: Prod = narrow

      assert(wide == 1 ><: "a" ><: Eye)
      assert(narrow == 1 ><: "a" ><: Eye)
    }

    it("applyProduct should keep exact Prod type for non-HList systems") {
      val original: Int ><: Eye = 1 ><: Eye
      val viaOf = of.applyProduct(original)
      val viaOfNarrow = ofNarrow.applyProduct(original)

      val _: Int ><: Eye = viaOf
      val _: Int ><: Eye = viaOfNarrow
      val _: Prod = viaOf
      val _: Prod = viaOfNarrow

      assert(viaOf == original)
      assert(viaOfNarrow == original)
    }

    it("FromProductOrValue should return Prod for non-HList systems") {
      val fromUnit = FromProductOrValue(())
      val _: Eye = fromUnit
      val _: Prod = fromUnit
      assert(fromUnit == Eye)

      val fromValue = FromProductOrValue(1)
      val _: Int ><: Eye = fromValue
      val _: Prod = fromValue
      assert(fromValue == 1 ><: Eye)

      val fromTupleValue = FromProductOrValue((1, "a"))
      val _: (Int, String) ><: Eye = fromTupleValue
      val _: Prod = fromTupleValue
      assert(fromTupleValue == (1, "a") ><: Eye)
    }

    it("FromProductOrValue should flatten tuples for HList-based monoidal systems") {
      import HListSystem.*

      val fromUnit = FromProductOrValue(())
      val _: Eye = fromUnit
      assert(fromUnit == Eye)

      val fromValue = FromProductOrValue(1)
      val _: Int ><: Eye = fromValue
      assert(fromValue == 1 ><: Eye)

      val fromTuple = FromProductOrValue((1, "a"))
      val _: Int ><: String ><: Eye = fromTuple
      assert(fromTuple == 1 ><: "a" ><: Eye)
    }
  }

  describe("Products.FromTupleX") {

    it("converts HNil to Eye for non-HList systems") {
      val converted = FromTupleX(shapeless.HNil)

      val _: Eye = converted
      val _: Prod = converted
      assert(converted == Eye)
    }

    it("converts HList to Prod for non-HList systems") {
      val list = 1 :: "a" :: shapeless.HNil
      val converted = FromTupleX(list)

      val _: Int ><: String ><: Eye = converted
      val _: Prod = converted
      assert(converted == 1 ><: "a" ><: Eye)
    }

    it("keeps existing Prod unchanged for non-HList systems") {
      val original: Int ><: Eye = 1 ><: Eye
      val converted = FromTupleX(original)

      val _: Int ><: Eye = converted
      val _: Prod = converted
      assert(converted == original)
    }

    it("keeps HList identity for HList-based systems") {
      import HListSystem.*

      val original = 1 :: "a" :: shapeless.HNil
      val converted = FromTupleX(original)

      val _: Int ><: String ><: Eye = converted
      assert(converted eq original)
    }
  }

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
