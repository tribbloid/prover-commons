package ai.acyclic.prover.commons.tuple

import shapeless.HNil

trait HListConverterMixin {
  self: Products.Monoidal =>

  // the following 2 objects are particularly troublesome, as shapeless HNil type is actually a trait, not a singleton
  // this is insane, hope future alternatives can get rid of it
  object FromHList extends Converter {
    val from: HLists.type = HLists
    val to: self.type = self

    implicit lazy val hnilCase: HNil |- to.Eye = at[HNil] { _ =>
      to.Eye
    }

    override def pointwise[T <: from.VBound & to.VBound]: from.Element[
      T
    ] => to.Element[T] =
      v => v.asInstanceOf[to.Element[T]]
  }

  object ToHList extends Converter {

    val from: self.type = self
    val to: HLists.type = HLists

    implicit lazy val hnilCase: from.Eye |- HNil = at[from.Eye] { _ =>
      HNil
    }

    override def pointwise[T <: from.VBound & to.VBound]: from.Element[T] => to.Element[
      T
    ] =
      v => v.asInstanceOf[to.Element[T]]
  }
}
