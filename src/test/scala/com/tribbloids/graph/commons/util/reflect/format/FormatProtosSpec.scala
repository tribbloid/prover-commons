package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.testlib.BaseSpec
import com.tribbloids.graph.commons.util.reflect.format.FormatOvrd.Only
import com.tribbloids.graph.commons.util.reflect.format.FormatProtos.{Hide, TransformText, Trials}
import com.tribbloids.graph.commons.util.reflect.format.beans.Beans
import com.tribbloids.graph.commons.util.viz.TypeViz
import shapeless.{::, HNil}

class FormatProtosSpec extends BaseSpec {

  import com.tribbloids.graph.commons.util.reflect.format.beans.Beans._

  describe("TransformText") {

    describe("On HidePackage") {

      val format = Formats.TypeInfo.HidePackage.recursively
      val viz = TypeViz.formattedBy(format)

      it("Parametric") {

        viz[XX[XX[YY.type]]].typeStr.shouldBe(
          "Beans.XX[Beans.XX[Beans.YY.type]]"
        )
      }

      it("Infix") {

        viz[
          XX[YY.type] :: XX[YY.type] :: XX[YY.type] :: HNil
        ].typeStr.shouldBe(
          "Beans.XX[Beans.YY.type] :: Beans.XX[Beans.YY.type] :: Beans.XX[Beans.YY.type] :: HNil"
        )
      }
    }

    describe(" ... with DeAlias") {

      val format = Formats.TypeInfo.DeAlias.HidePackage.recursively
      val viz = TypeViz.formattedBy(format)

      it("Parametric") {

        viz[T1].typeStr.shouldBe(
          "Beans.XX[Beans.XX[Beans.YY.type]]"
        )
      }

      it("Infix") {

        viz[T2].typeStr.shouldBe(
          "Beans.XX[Beans.YY.type] :: Beans.XX[Beans.YY.type] :: Beans.XX[Beans.YY.type] :: HNil"
        )
      }

      it("Inner Type") {
        viz[T3].typeStr.shouldBe(
          "Beans.XX[Beans.XX[Beans.YY.type]]#ZZ[Beans.YY.type]"
        )
      }
    }

    describe(" ... with Trials") {

      val before = Formats.TypeInfo.DeAlias
      val after = Trials(
        Only,
        Hide.Package(before)
      )

      val format = TransformText(after)
      val viz = TypeViz.formattedBy(format)

      it("Parametric") {
        viz[Ovrd.Ref].typeStr.shouldBe(
          "Beans.XX[Beans.XX[3]]"
        )
      }
    }

    describe(" ... with EnableOvrd") {

      val before = Formats.TypeInfo.DeAlias
      val after = EnableOvrd(Hide.Package(before))

      val format = TransformText(after)
      val viz = TypeViz.formattedBy(format)

      it("Parametric") {

        viz[Ovrd.Plain].typeStr.shouldBe(
          "Beans.XX[Beans.XX[3]]"
        )
      }

      it(" ... with fallback") {
        viz[Ovrd.Ref].typeStr.shouldBe(
          "Beans.XX[Beans.XX[Int(3)]]"
        )
      }

      it("Infix") {

        viz[Ovrd.Plain].typeStr.shouldBe(
          "Beans.XX[Beans.XX[3]]"
        )
      }

      it(" ... mixed 1") {
        viz[Ovrd.T2].typeStr.shouldBe(
          "Beans.XX[3] :: Beans.XX[Int(3)] :: HNil"
        )
      }

      it(" ... mixed 2") {
        viz[Ovrd.T3].typeStr.shouldBe(
          "Beans.XX[3] :: Beans.XX[3] :: Beans.XX[Int(3)] :: HNil"
        )
      }

      it(" ... mixed 3") {
        viz[Ovrd.T4].typeStr.shouldBe(
          "Beans.XX[3] :: Beans.XX[3] :: Beans.XX[3] :: Beans.XX[Int(3)] :: HNil"
        )
      }
    }

    describe("On HideOwner") {

      val format = Formats.TypeInfo.DeAlias.HideOwner.recursively
      val viz = TypeViz.formattedBy(format)

      it("Parametric") {

        viz[T1].typeStr.shouldBe(
          "XX[XX[YY.type]]"
        )
      }

      it("Infix") {

        viz[
          T2
        ].typeStr.shouldBe(
          "XX[YY.type] :: XX[YY.type] :: XX[YY.type] :: HNil"
        )
      }

      it("Inner Type") {
        viz[T3].typeStr.shouldBe(
          "XX[XX[YY.type]]#ZZ[YY.type]"
        )
      }

      it("directly under package") {

        viz[
          Beans
        ].typeStr.shouldBe(
          "Beans"
        )
      }
    }
  }

}

object FormatProtosSpec {

  trait SS {

    trait TT
  }
}
