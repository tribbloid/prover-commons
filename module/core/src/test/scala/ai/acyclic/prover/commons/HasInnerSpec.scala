package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.testlib.BaseSpec

class HasInnerSpec extends BaseSpec {

  describe("Inner can") {

    import HasInnerSpec.*

    val a1 = A()
    val a1_ = A()
    val a2 = A(2)

    val b1 = a1.B()
    val b1_ = a1_.B()
    val b2 = a2.B()

    it("access outer") {

      assert(b1.outer == a1)
    }

    it("use outer for hashcode") {

      assert(b1.hashCode() == b1_.hashCode())
      assert(b1.hashCode != b2.hashCode)
    }

    it("use outer for equality") {

      assert(b1 == b1_)
      assert(b1 != b2)
    }

  }

  describe("vanilla Scala inner case class cannot") {

    import ai.acyclic.prover.commons.HasInnerSpec.Vanilla.*

    val a1 = A()
    val a1_ = A()
    val a2 = A(2)

    val b1 = a1.B()
    val b1_ = a1_.B()
    val b2 = a2.B()

    it("use outer for hashcode") {

      assert(b1.hashCode() == b1_.hashCode())
      assert(b1.hashCode == b2.hashCode) // vanilla failed here
    }

    it("use outer for equality") {

      assert(b1 != b1_) // vanilla failed here
      assert(b1 != b2)
    }
  }

  describe("type class can summoned from inner companion object") {

    import HasInnerSpec.*

    // path as in path-dependent type
    it("if the outer is a path") {

      val a1 = A()
      val b1 = a1.B(1)
      implicitly[TypeCls[b1.type]]
      implicitly[TypeCls[a1.B]]
    }

    it("NOT if the outer is not a path") {

      def a1 = A()
      a1.B(1)

      //    implicitly[TypeCls[mm.type]]

      shouldNotCompile(
        """implicitly[TypeCls[mm.type]]"""
      )
    }

    it("indirectly, if the inner is a path") {

      def a1 = A()
      val b1 = a1.B(1)

      implicitly[TypeCls[b1.outer.B]] // TODO: how to make the compiler automatically realise it?
    }
  }

}

object HasInnerSpec {

  class TypeCls[T]

  case class A(v: Int = 1) extends HasInner {

    case class B(v: Int = 1) extends _Inner {}

    object B {

      implicit def getTypeC[T <: B]: TypeCls[T] = new TypeCls[T]
    }
  }

  object Vanilla {

    case class A(v: Int = 1) {

      case class B(v: Int = 1) {}
    }
  }
}
