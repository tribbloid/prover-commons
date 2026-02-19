package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec

class TransformationSpec extends BaseSpec {

  import Fixture.*

  object MyTransformation extends Transformation {
    object source extends Transformation.Schema {
      override val system: SrcSystem.type = SrcSystem
      override type G[T] = T
    }
    object target extends Transformation.Schema {
      override val system: TgtSystem.type = TgtSystem
      override type G[T] = Option[T]
    }

    override def pointwise[HEAD](v: source.system.Element[source.G[HEAD]]): target.system.Element[target.G[HEAD]] =
      Option(v)
  }

  describe("Transformation") {

    it("should transform empty product") {
      val src = SrcSystem.Eye
      val dst = MyTransformation(src)
      assert(dst == TgtSystem.Eye)
    }

    it("should transform single element product") {
      val src = SrcSystem.><:(1, SrcSystem.Eye)
      // Implicit resolution
      import MyTransformation.*

      val dst = MyTransformation(src)

      assert(dst == TgtSystem.><:(Some(1), TgtSystem.Eye))
    }

    it("should transform multi-element product") {
      val src = SrcSystem.><:("a", SrcSystem.><:(1, SrcSystem.Eye))
      import MyTransformation.*

      val dst = MyTransformation(src)

      assert(dst == TgtSystem.><:(Some("a"), TgtSystem.><:(Some(1), TgtSystem.Eye)))
    }
  }
}
