package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.testlib.BaseSpec
import ai.acyclic.prover.commons.tuple.backbone.RecursiveHeapBackbone

class ConverterSpec extends BaseSpec {

  object B1 extends RecursiveHeapBackbone {
    override type VBound = Any
  }

  object B2 extends RecursiveHeapBackbone {
    override type VBound = Any
  }

  object Conv extends Converter {
    override val from: B1.type = B1
    override val to: B2.type = B2
  }

  describe("Converter") {}
}
