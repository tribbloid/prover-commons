package com.tribbloids.graph.commons.util.reflect.extension

import com.tribbloids.graph.commons.testlib.BaseSpec
import com.tribbloids.graph.commons.util.reflect.Reflection
import com.tribbloids.graph.commons.util.reflect.format.HasTypeInfo.{~~, ConstV}
import com.tribbloids.graph.commons.util.reflect.format.{HasTypeInfo, InfoOvrd, TypeFormat}
import com.tribbloids.graph.commons.util.viz.TypeViz
import shapeless.Witness

class InfoOvrdSpec extends BaseSpec {

  import InfoOvrdSpec._

  val format: TypeFormat = InfoOvrd(
    TypeFormat.DeAlias(TypeFormat.TypeInternal)
  )

  val viz: TypeViz[Reflection.Runtime.type] = TypeViz.withFormat(format)

  describe(ConstV.toString) {

    it("1") {

      viz[ConstV[3]].typeStr.shouldBe("3")
    }

    it("2") {

      val o = WConstV(3)
      viz.infer(o).typeStr.shouldBe("3")
    }

    it("3") {

      viz[ConstV[global.type]].typeStr.shouldBe(
        s"${InfoOvrdSpec.getClass.getCanonicalName.stripSuffix("$")}.global.type"
      )
    }

    it("4") {

      val o3 = WConstV(global)

      viz
        .infer(o3)
        .typeStr
        .shouldBe(
          s"${InfoOvrdSpec.getClass.getCanonicalName.stripSuffix("$")}.global.type"
        )
    }

    it("5") {

      val local = 3
      val o2 = WConstV(local)

      viz.infer(o2).typeStr.shouldBe("local.type")
    }
  }

  describe(~~.toString) {

    it("1") {
      viz[W_~~[3]].typeStr.shouldBe("3 3 Int(3): UniqueConstantType")
    }

    it("2") {

      val o = W_~~(3)
      viz.infer(o).typeStr.shouldBe("3 3 Int(3): UniqueConstantType")
    }
  }
}

object InfoOvrdSpec {

  val global = 3

  class WConstV[T <: Int](w: Witness.Aux[T]) extends HasTypeInfo {

    override type _TypeInfo = ConstV[T]
  }
  object WConstV {

    def apply(w: Witness.Lt[Int]) = new WConstV[w.T](w)
  }

  class W_~~[T <: Int](w: Witness.Aux[T]) extends HasTypeInfo {

    override type _TypeInfo = ConstV[T] ~~ WConstV[T] ~~ T
  }
  object W_~~ {

    def apply(w: Witness.Lt[Int]) = new W_~~[w.T](w)
  }
}
