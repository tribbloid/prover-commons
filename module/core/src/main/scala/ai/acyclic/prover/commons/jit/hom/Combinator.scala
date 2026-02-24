package ai.acyclic.prover.commons.jit.hom

object Combinator {

  // for conversion between extensionally equal types (but cannot be represented in the current type system)

  // TODO: not all are defined, will add more in the following order:
  //  - B/C: used in autograd
  //  - delta/gamma: used in interaction combinators
  //  - S: used in STLC
  //  - Y: don't know what is it for

  object SKI {

    val K = Const.Lazy
    val I = Fn.Identity
  }

  object BCKW {

    val B = Fn.Mapped
    val C = Fn.Flipped
  }

  object InteractionNet {

    val Delta = Fn.PointwiseZip
    val Gamma = Fn.Duplicate
  }
}
