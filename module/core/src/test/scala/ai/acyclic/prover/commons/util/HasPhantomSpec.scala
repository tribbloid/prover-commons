package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.compat.TupleXEmpty
import ai.acyclic.prover.commons.jit.eval.Args
import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.verification.Verify

import scala.reflect.classTag

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

}

class HasPhantomSpec extends BaseSpec {

  import HasPhantomSpec.*

  describe("HasPhantom.Phantom.summonConcrete") {

    it("instantiates a concrete phantom with a nullary constructor") {
      val phantom = App.Phantom.summonConcrete[ConcretePhantom](classTag[ConcretePhantom])

      assert(phantom.value == 42)

      val out: phantom.Out = "ok"
      assert(out == "ok")
    }

    it("can instantiate a phantom through a private nullary constructor") {
      val phantom = App.Phantom.summonConcrete[PrivateCtorPhantom](classTag[PrivateCtorPhantom])

      assert(phantom.value == 7)
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
        App.Phantom.summonConcrete[NoNullaryCtorPhantom](classTag[NoNullaryCtorPhantom])
      }

      assert(err.getMessage.contains("nullary constructor"))
      assert(err.getMessage.contains(classOf[NoNullaryCtorPhantom].getName))
    }

    it("fails when the phantom subtype is abstract") {
      val err = intercept[IllegalStateException] {
        App.Phantom.summonConcrete[AbstractPhantom](classTag[AbstractPhantom])
      }

      assert(err.getMessage.contains(classOf[AbstractPhantom].getName))
    }
  }
}
