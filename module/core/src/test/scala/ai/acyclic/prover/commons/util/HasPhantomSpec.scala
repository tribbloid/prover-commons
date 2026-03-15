package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.verification.Verify

object HasPhantomSpec {

  object App extends HasPhantom {
    trait NotPhantom
  }

  class ConcretePhantom extends App.Phantom {
    type Out = String
    val value: Int = 42
  }

  class PrivateCtorPhantom private () extends App.Phantom {
    type Out = Int
    val value: Int = 7
  }

  object PrivateCtorPhantom {
    val compileTimeUse: PrivateCtorPhantom = new PrivateCtorPhantom()
  }

  abstract class AbstractPhantom extends App.Phantom {
    type Out = String
    def value: Int
  }

  class NoNullaryCtorPhantom(val value: Int) extends App.Phantom {
    type Out = Int
  }

  class OneArgCtorPhantom(
      implicit
      ttg: ai.acyclic.prover.commons.WeakTypeTag[?]
  ) extends App.Phantom(ttg) {
    type Out = String
    val value: Int = 100
    val savedTtg = ttg
  }

  object ConcreteObjectPhantom extends App.Phantom {
    type Out = String

    val value: Int = 99
  }

}

class HasPhantomSpec extends BaseSpec {

  import HasPhantomSpec.*

  describe("HasPhantom.Phantom.summonConcrete") {

    it("instantiates a concrete phantom with a nullary constructor") {
      val phantom = App.Phantom.summonConcrete[ConcretePhantom]

      assert(phantom.out.value == 42)

      val out: phantom.out.Out = "ok"
      assert(out == "ok")
    }

    it("can instantiate a phantom through a private nullary constructor") {
      val phantom = App.Phantom.summonConcrete[PrivateCtorPhantom]

      assert(phantom.out.value == 7)
    }

    it("instantiates a phantom object and preserves its runtime members") {
      val phantom =
        App.Phantom.summonConcrete[ConcreteObjectPhantom.type]

      val _: ConcreteObjectPhantom.type = phantom.out
      val out: phantom.out.Out = "ok"

      assert(phantom.out.value == 99)
      assert(out == "ok")
    }

    it("rejects non-phantom types at compile time") {
      Verify.typeError(
        "ai.acyclic.prover.commons.util.HasPhantomSpec.App.Phantom.summonConcrete[ai.acyclic.prover.commons.util.HasPhantomSpec.App.NotPhantom](scala.reflect.classTag[ai.acyclic.prover.commons.util.HasPhantomSpec.App.NotPhantom])"
      )

      Verify.typeError(
        "ai.acyclic.prover.commons.util.HasPhantomSpec.App.Phantom.summonConcrete[Int](scala.reflect.classTag[Int])"
      )
    }

    it("fails when no nullary constructor exists") {
      val err = intercept[IllegalArgumentException] {
        App.Phantom.summonConcrete[NoNullaryCtorPhantom]
      }

      assert(err.getMessage.contains("nullary constructor"))
      assert(err.getMessage.contains(classOf[NoNullaryCtorPhantom].getName))
    }

    it("fails when the phantom subtype is abstract") {
      val err = intercept[IllegalStateException] {
        App.Phantom.summonConcrete[AbstractPhantom]
      }

      assert(err.getMessage.contains(classOf[AbstractPhantom].getName))
    }
  }

  describe("HasPhantom.Phantom.summonConcrete with 1-arg constructor") {

    it("instantiates a concrete phantom with a 1-arg WeakTypeTag constructor") {
      val phantom = App.Phantom.summonConcrete[OneArgCtorPhantom]

      assert(phantom.out.value == 100)
      assert(phantom.out.savedTtg != null)

      val out: phantom.out.Out = "ok"
      assert(out == "ok")
    }
  }
}
