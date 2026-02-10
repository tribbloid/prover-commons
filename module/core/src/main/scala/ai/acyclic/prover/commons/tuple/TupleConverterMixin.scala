package ai.acyclic.prover.commons.tuple

import ai.acyclic.prover.commons.jit.hom.Hom
import ai.acyclic.prover.commons.tuple.Tuples.><:
import shapeless.HNil

trait TupleConverterMixin {
  self: BTuples =>

  // the following 2 objects are particularly troublesome, as shapeless HNil type is actually a trait, not a singleton
  // this is insane, hope future alternatives can get rid of it
  object FromTuple extends Converter {
    val from: Tuples.type = Tuples
    val to: self.type = self

    implicit lazy val hnilCase: HNil |- to.Empty = at[HNil] { _ =>
      to.Empty
    }
  }

  object ToTuple extends Converter {

    val from: self.type = self
    val to: Tuples.type = Tuples

    implicit lazy val hnilCase: from.Empty |- HNil = at[from.Empty] { _ =>
      HNil
    }
  }
}
