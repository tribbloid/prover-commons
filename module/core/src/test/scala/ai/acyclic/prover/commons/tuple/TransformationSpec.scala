package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec

class TransformationSpec extends BaseSpec {

  object SrcSystem extends Products.Monoidal {
    type VBound = Any
    type Element[T <: VBound] = T

    sealed trait Prod
    case object Eye extends Prod
    type Eye = Eye.type

    case class ><:[L <: VBound, TAIL <: Prod](head: L, tail: TAIL) extends Prod
  }

  object DstSystem extends Products.Monoidal {
    type VBound = Any
    type Element[T <: VBound] = Option[T]

    sealed trait Prod
    case object Eye extends Prod
    type Eye = Eye.type

    case class ><:[L <: VBound, TAIL <: Prod](head: Option[L], tail: TAIL) extends Prod
  }

  object MyTransformation extends Transformation {
    object from extends Transformation.Schema {
      override val system: SrcSystem.type = SrcSystem
      override type G[T] = T
    }
    object to extends Transformation.Schema {
      override val system: DstSystem.type = DstSystem
      override type G[T] = Option[T]
    }

    implicit override def emptyCase: SrcSystem.Eye |- DstSystem.Eye =
      at(_ => DstSystem.Eye)

    override def pointwise[HEAD](v: from.G[HEAD]): to.G[HEAD] = Option(v)
  }

  describe("Transformation") {

    it("should transform empty product") {
      val src = SrcSystem.Eye
      val dst = MyTransformation.emptyCase(src)
      assert(dst == DstSystem.Eye)
    }

    it("should transform single element product") {
      val src = SrcSystem.><:(1, SrcSystem.Eye)
      // Implicit resolution
      import MyTransformation._

      // We explicitly summon the implicit to verify it can be found
      // Using Impl type alias directly
      val transform =
        implicitly[MyTransformation.Impl[SrcSystem.><:[Int, SrcSystem.Eye], DstSystem.><:[Option[Int], DstSystem.Eye]]]
      val dst = transform(src)

      assert(dst == DstSystem.><:(Some(1), DstSystem.Eye))
    }

    it("should transform multi-element product") {
      val src = SrcSystem.><:("a", SrcSystem.><:(1, SrcSystem.Eye))
      import MyTransformation._

      val dst = MyTransformation(src)

      assert(dst == DstSystem.><:(Some("a"), DstSystem.><:(Some(1), DstSystem.Eye)))
    }
  }
}
