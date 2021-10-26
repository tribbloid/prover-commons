package org.shapesafe.graph.commons.util.reflect.format

import org.shapesafe.graph.commons.testlib.BaseSpec
import org.shapesafe.graph.commons.util.reflect.format.Formats1.{RecursiveForm, Trials}
import org.shapesafe.graph.commons.util.reflect.format.beans.Beans
import org.shapesafe.graph.commons.util.viz.TypeViz
import shapeless.{::, HNil}

class Formats1Spec extends BaseSpec {

  import org.shapesafe.graph.commons.util.reflect.format.beans.Beans._

  describe("Transform text recursively") {

    describe("On HidePackage") {

      val format = Formats0.TypeInfo.HidePackage.recursively
      val viz = TypeViz.withFormat(format)

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

      val format = Formats0.TypeInfo.HidePackage.recursively.DeAlias
      val viz = TypeViz.withFormat(format)

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

      val base = Formats0.TypeInfo

      val transformer = { v: TypeFormat =>
        val firstTrial = BacktrackingDummy
        val secondTrial = v.HidePackage.DeAlias
        Trials(
          firstTrial,
          secondTrial
        )
      }

      val format = RecursiveForm(base, transformer)

      val viz = TypeViz.withFormat(format)

      it("Parametric") {
        viz[Ovrd.Ref].typeStr.shouldBe(
          "Beans.XX[Beans.XX[Int(3)]]"
        )
      }
    }

    describe(" ... with EnableOvrd") {

      val base = Formats0.TypeInfo

      val transformer = { v: TypeFormat =>
        val firstTrial = BacktrackingDummy
        val secondTrial = EnableOvrd(v.HidePackage.DeAlias)
        Trials(
          firstTrial,
          secondTrial
        )
      }

      val format = RecursiveForm(base, transformer)

      val viz = TypeViz.withFormat(format)

      it("Parametric") {

        viz[Ovrd.Plain].typeStr.shouldBe(
          "Beans.XX[Beans.XX[3]]"
        )

        viz[Ovrd.Plain2].typeStr.shouldBe(
          "Beans.XX[Beans.YY[3,3]]"
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

    describe("On HideStatic") {

      val format = Formats0.TypeInfo.HideStatic.recursively.DeAlias
      val viz = TypeViz.withFormat(format)

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

object Formats1Spec {

  trait SS {

    trait TT
  }
}
