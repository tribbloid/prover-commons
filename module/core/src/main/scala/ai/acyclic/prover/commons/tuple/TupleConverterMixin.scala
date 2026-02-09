package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.tuple.Tuples.><:
import shapeless.HNil

trait TupleConverterMixin {
  self: BTuples =>

  object FromTuple extends Converter {
    val from: Tuples.type = Tuples
    val to: self.type = self
  }

  object ToTuple extends Converter {

    val from: self.type = self
    val to: Tuples.type = Tuples
  }
}
