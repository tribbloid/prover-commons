package ai.acyclic.prover.commons.viz.format

import ai.acyclic.prover.commons.compat.XInt
import ai.acyclic.prover.commons.meta.ScalaReflection
import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.viz.TypeViz
import ai.acyclic.prover.commons.viz.format.FormatOvrd.{~~, SingletonName}
import ai.acyclic.prover.commons.viz.format.Formats0.{ClassName, TypeImpl}

class FormatOvrdSpec extends BaseSpec {

  import FormatOvrdSpec.*

  val format: TypeFormat = EnableOvrd(TypeImpl.DeAlias)

  val viz = TypeViz.withFormat(format)

  describe("fallback") {

    it("1") {

      viz[String].typeStr.shouldBe("String: ClassNoArgsTypeRef")
    }

    it("2") {
      val tt = ScalaReflection.universe.typeOf[Undefined[Int]]

      viz[Undefined[Int]].typeStr.shouldBe(
        s"$tt: ClassArgsTypeRef"
      )
    }
  }

  describe(SingletonName.toString) {

    it("1") {

      viz[SingletonName[3]].typeStr.shouldBe("3")
    }

    it("2") {

      val o = W_Singleton(3)
      viz[o._Info].typeStr.shouldBe("3")
    }

    it("3") {

//      viz[Only[global.type]].should_=:=()

      viz[SingletonName[global.type]].typeStr.shouldBe(
        "3"
      )
    }

    it("4") {

      val o3: W_Singleton[3] = W_Singleton(global)

      viz[o3._Info].typeStr
        .shouldBe(
          "3"
        )
    }

    it("fallback") {

      val local = 3
      val o2 = W_Singleton(local)

      viz[o2._Info].typeStr
        .shouldBe(
          s"${classOf[SingletonName[?]].getCanonicalName}[local.type]: ClassArgsTypeRef"
        )

      viz[W_Singleton[XInt]#_Info].typeStr.shouldBe(
        s"${classOf[SingletonName[?]].getCanonicalName}[Int with Singleton]: ClassArgsTypeRef"
      )

      viz[W_Singleton[XInt]#_InfoWFallback].typeStr.shouldBe(
        s"${FormatOvrdSpec.getClass.getCanonicalName.stripSuffix("$")}.W_Singleton"
      )
    }
  }

  describe("~~") {

    it("1") {
      viz[W_~~[3]#_Info].typeStr.shouldBe("3 3 Int(3): UniqueConstantType")
    }

    it("2") {

      val o = W_~~(3)
      viz[o._Info].typeStr.shouldBe("3 3 Int(3): UniqueConstantType")
    }
  }
}

object FormatOvrdSpec {

  final val global = 3

  class Undefined[T]

  class W_Singleton[T <: XInt](w: T) {

    type _Info = SingletonName[T]

    type _InfoWFallback = _Info & ClassName[this.type]
  }
  object W_Singleton {

    def apply(w: XInt) = new W_Singleton[w.type](w)
  }

  class W_~~[T <: XInt](w: T) {

    type _Info = SingletonName[T] ~~ W_Singleton[T]#_Info ~~ T
  }
  object W_~~ {

    def apply(w: XInt) = new W_~~[w.type](w)
  }
}
