package ai.acyclic.prover.commons.reflect.format

import ai.acyclic.prover.commons.reflect.Reflection
import ai.acyclic.prover.commons.reflect.format.FormatOvrd.{~~, SingletonName}
import ai.acyclic.prover.commons.reflect.format.Formats0.{ClassName, TypeImpl}
import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.viz.TypeViz
import shapeless.Witness

class FormatOvrdSpec extends BaseSpec {

  import FormatOvrdSpec._

  val format: TypeFormat = EnableOvrd(TypeImpl.DeAlias)

  val viz = TypeViz.withFormat(format)

  describe("fallback") {

    it("1") {

      viz[String].typeStr.shouldBe("String: ClassNoArgsTypeRef")
    }

    it("2") {
      val tt = Reflection.Runtime.universe.typeOf[Undefined[Int]]

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
          s"${classOf[SingletonName[_]].getCanonicalName}[local.type]: ClassArgsTypeRef"
        )

      viz[W_Singleton[Int]#_Info].typeStr.shouldBe(
        s"${classOf[SingletonName[_]].getCanonicalName}[Int]: ClassArgsTypeRef"
      )

      viz[W_Singleton[Int]#_InfoWFallback].typeStr.shouldBe(
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

  class W_Singleton[T <: Int](w: Witness.Aux[T]) {

    type _Info = SingletonName[T]

    type _InfoWFallback = _Info with ClassName[this.type]
  }
  object W_Singleton {

    def apply(w: Witness.Lt[Int]) = new W_Singleton[w.T](w)
  }

  class W_~~[T <: Int](w: Witness.Aux[T]) {

    type _Info = SingletonName[T] ~~ W_Singleton[T]#_Info ~~ T
  }
  object W_~~ {

    def apply(w: Witness.Lt[Int]) = new W_~~[w.T](w)
  }
}
