package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.testlib.BaseSpec
import com.tribbloids.graph.commons.util.reflect.format.DerivedFormats.{HidePackage, TransformUp}
import com.tribbloids.graph.commons.util.viz.TypeViz
import shapeless.{::, HNil}

class DerivedFormatsSpec extends BaseSpec {

  import com.tribbloids.graph.commons.util.reflect.format.beans._

  describe("HidePackages") {

    val format = Formats.TypeInfo.HidePackages
    val viz = TypeViz.formattedBy(format)

    it("Parametric") {

      viz[XX[XX[YY.type]]].typeStr.shouldBe(
        "XX[XX[YY.type]]"
      )
    }

    it("Infix") {

      viz[
        XX[YY.type] :: XX[YY.type] :: XX[YY.type] :: HNil
      ].typeStr.shouldBe(
        "XX[YY.type] :: XX[YY.type] :: XX[YY.type] :: HNil"
      )
    }
  }

  describe(" ... on DeAlias") {

    val format = Formats.TypeInfo.DeAlias.HidePackages
    val viz = TypeViz.formattedBy(format)

    it("Parametric") {

      viz[T1].typeStr.shouldBe(
        "XX[XX[YY.type]]"
      )
    }

    it("Infix") {

      viz[T2].typeStr.shouldBe(
        "XX[YY.type] :: XX[YY.type] :: XX[YY.type] :: HNil"
      )
    }
  }

  describe(" ... on EnableOvrd") {

    val before = Formats.TypeInfo.DeAlias
    val after = EnableOvrd(HidePackage(before))

    val format = TransformUp(before, after)
    val viz = TypeViz.formattedBy(format)

    it("Parametric") {

      viz[Ovrd.Plain].typeStr.shouldBe(
        "XX[XX[3]]"
      )
    }

    it(" ... with fallback") {
      viz[Ovrd.Ref].typeStr.shouldBe(
        "XX[XX[Int(3)]]"
      )
    }

    it("Infix") {

      viz[Ovrd.Plain].typeStr.shouldBe(
        "XX[XX[3]]"
      )
    }

    it(" ... mixed 1") {
      viz[Ovrd.T2].typeStr.shouldBe(
        "XX[3] :: XX[Int(3)] :: HNil"
      )
    }

    it(" ... mixed 2") {
      viz[Ovrd.T3].typeStr.shouldBe(
        "XX[3] :: XX[3] :: XX[Int(3)] :: HNil"
      )
    }

    it(" ... mixed 3") {
      viz[Ovrd.T4].typeStr.shouldBe(
        "\nXX[3] :: XX[3] :: XX[3] :: XX[Int(3)] :: HNil"
      )
    }
  }
}

object DerivedFormatsSpec {}
