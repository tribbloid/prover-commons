package ai.acyclic.prover.commons.jit.poly

import ai.acyclic.prover.commons.testlib.BaseSpec

object DepSigmaSpec {

  sealed trait Term
  final case class I(value: Int) extends Term
  final case class S(value: String) extends Term
  final case class Missing(value: Double) extends Term
  trait Show[-T] extends (T => String)
  implicit val iShow: Show[I] = value => s"i:${value.value}"
  implicit val sShow: Show[S] = value => s"s:${value.value}"

  object Fixture extends DepSigma {
    type Left = Term
    type DepRight[T <: Left] = Show[T]
    def show[T <: Left](value: Repr[T]): String = value.right(value.left)
  }
}

class DepSigmaSpec extends BaseSpec {

  import DepSigmaSpec.*
  import DepSigmaSpec.Fixture.*

  describe("DepSigma") {

    it("unpacks a value with its matching dependent evidence") {
      val repr: Repr[I] = I(2)

      assert(repr.left == I(2))
      assert(repr.right eq iShow)
    }

    it("preserves the concrete subtype when consuming Repr") {
      def showInt(value: Repr[I]): String = value.right(value.left)
      def showString(value: Repr[S]): String = value.right(value.left)

      val intResult = showInt(I(2))
      val textResult = showString(S("abc"))

      assert(intResult == "i:2")
      assert(textResult == "s:abc")
    }

    it("can apply functions that require Repr directly to matching left values") {
      val renderedInt = show(I(3))
      val renderedText = show(S("xyz"))

      assert(renderedInt == "i:3")
      assert(renderedText == "s:xyz")
    }

    it("does not unpack when the dependent evidence is missing") {
      shouldNotCompile("""
import ai.acyclic.prover.commons.jit.poly.DepSigmaSpec.Fixture.*
val _: Repr[Missing] = Missing(1.0)
""")
    }

    it("does not unpack a widened left value without upper-bound evidence") {
      shouldNotCompile("""
import ai.acyclic.prover.commons.jit.poly.DepSigmaSpec.Fixture.*
def showWidened(value: Left): String = show(value)
""")
    }
  }
}
