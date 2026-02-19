package ai.acyclic.prover.commons.tuple


trait HListConverterMixin {
  self: Schemata.Monoidal =>

  // the following 2 objects are particularly troublesome, as shapeless HNil type is actually a trait, not a singleton
  // this is insane, hope future alternatives can get rid of it
//  object FromHList extends Converter {
//    val from: HLists.type = HLists
//    val to: self.type = self
//
//    override def pointwise[T <: from.VBound & to.VBound]: T => to.Element[T] =
//      v => v.asInstanceOf[to.Element[T]]
//
//    implicit lazy val hnilCase: HNil |- to.Eye = at[HNil] { _ =>
//      to.Eye
//    }
//  }
//
//  object ToHList extends Converter {
//
//    val from: self.type = self
//    val to: HLists.type = HLists
//
//    override def pointwise[T <: from.VBound & to.VBound]: from.Element[T] => to.Element[
//      T
//    ] =
//      v => v.asInstanceOf[to.Element[T]]
//
//    implicit lazy val hnilCase: from.Eye |- HNil = at[from.Eye] { _ =>
//      HNil
//    }
//  }
}
