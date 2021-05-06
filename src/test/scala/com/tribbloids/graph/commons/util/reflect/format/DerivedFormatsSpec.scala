package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.testlib.BaseSpec
import com.tribbloids.graph.commons.util.viz.TypeViz
import shapeless.{::, HNil}

class DerivedFormatsSpec extends BaseSpec {

  import DerivedFormatsSpec._

  describe("HidePackages") {

    val format = Formats.TypeInfo.HidePackages
    val viz = TypeViz.formattedBy(format)

    it("on TypeInfo") {

      viz[Deep[Deep[DerivedFormatsSpec.type]]].typeStr.shouldBe(
        "DerivedFormatsSpec.Deep[DerivedFormatsSpec.Deep[DerivedFormatsSpec.type]]"
      )
    }

    it(" ... with Infix") {

      viz[
        Deep[DerivedFormatsSpec.type] :: Deep[DerivedFormatsSpec.type] :: Deep[DerivedFormatsSpec.type] :: HNil
      ].typeStr.shouldBe(
        """
          |DerivedFormatsSpec.Deep[DerivedFormatsSpec.type] :: DerivedFormatsSpec.Deep[DerivedFormatsSpec.type] :: DerivedFormatsSpec.Deep[DerivedFormatsSpec.type] :: shapeless.HNil
        """.stripMargin
      )
    }
  }

  describe(" ... with DeAlias") {

    val format = Formats.TypeInfo.DeAlias.HidePackages
    val viz = TypeViz.formattedBy(format)

    it("on TypeInfo") {

      viz[T1].typeStr.shouldBe(
        "DerivedFormatsSpec.Deep[DerivedFormatsSpec.Deep[DerivedFormatsSpec.type]]"
      )
    }

    it(" ... with Infix") {

      viz[T2].typeStr.shouldBe(
        """
          |DerivedFormatsSpec.Deep[DerivedFormatsSpec.type] :: DerivedFormatsSpec.Deep[DerivedFormatsSpec.type] :: DerivedFormatsSpec.Deep[DerivedFormatsSpec.type] :: shapeless.HNil
        """.stripMargin
      )
    }
  }
}

object DerivedFormatsSpec {

  trait Deep[T]

  type T1 = Deep[Deep[DerivedFormatsSpec.type]]

  type T2 = Deep[DerivedFormatsSpec.type] :: Deep[DerivedFormatsSpec.type] :: Deep[DerivedFormatsSpec.type] :: HNil

}
