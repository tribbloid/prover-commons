package ai.acyclic.prover.commons.jit.tracing

import ai.acyclic.prover.commons.debug.SrcDefinition
import ai.acyclic.prover.commons.util.{TupleCons, TupleUnpack}

trait CanReifyMany_Imp0 {

  implicit def identity[O]: CanReifyMany.Aux[O, O] = new CanReifyMany[O] {
    type Out = O
    override def reify(inputs: O)(
        implicit
        defAt: SrcDefinition
    ): O = inputs
    override def const(values: O): O = values
  }

  implicitly[Const[Int] <:< Input[Int]]

  implicit def unpack[
      T,
      Head,
      Tail,
      HO,
      TLO,
      O
  ](
      implicit
      unpack: TupleUnpack.Aux[T, Head, Tail],
      ev: Head <:< Input[HO], // TODO: this can be avoided, there is only 1 Head
      tReify: CanReifyMany.Aux[Tail, TLO],
      packO: TupleCons.Aux[HO, TLO, O],
      bound: Const[HO] <:< Head // TODO: ditto
  ): CanReifyMany.Aux[T, O] = new CanReifyMany[T] {
    type Out = O

    override def reify(inputs: T)(
        implicit
        defAt: SrcDefinition
    ): O = {
      val (hRaw, t): (Head, Tail) = unpack.unpack(inputs)
      val h: Input[HO] = ev(hRaw)
      val ho = h.reify(defAt)
      val tlo = tReify.reify(t)
      packO.pack(ho, tlo)
    }

    override def const(values: O): T = {
      // TODO: this logic is inverted and very convoluted, it is better to rewrite TupleCons to be bidirectional
      //      val (ho, tlo) = unpackO.unpack(values)
      //      val h: Const[HO] = Const(ho)
      //      val t = tReify.const(tlo)
      //      val _h: Head = bound(h)
      //      unpack.pack(_h, t)
      ???
    }
  }
}
