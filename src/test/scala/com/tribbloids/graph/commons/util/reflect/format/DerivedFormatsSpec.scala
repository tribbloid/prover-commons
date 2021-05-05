package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.testlib.BaseSpec
import com.tribbloids.graph.commons.util.viz.TypeViz

class DerivedFormatsSpec extends BaseSpec {

  import DerivedFormatsSpec._

  describe("HidePackage") {

    val format = Formats.TypeInfo.DeAlias.HidePackage
    val viz = TypeViz.formattedBy(format)

    it("on TypeInfo") {

      viz[T1].typeStr.shouldBe(
        """
          |DerivedFormatsSpec.Deep[com.tribbloids.graph.commons.util.reflect.format.DerivedFormatsSpec.Deep[com.tribbloids.graph.commons.util.reflect.format.DerivedFormatsSpec.type]]
          |""".stripMargin
      )
    }
  }

  describe("HidePackages") {

    val format = Formats.TypeInfo.DeAlias.HidePackages
    val viz = TypeViz.formattedBy(format)

    it("on TypeInfo") {

      viz[T1].typeStr.shouldBe()
    }

    it(" ... with Infix") {}
  }
}

object DerivedFormatsSpec {

  trait Deep[T]

  type T1 = Deep[Deep[DerivedFormatsSpec.type]]
}
