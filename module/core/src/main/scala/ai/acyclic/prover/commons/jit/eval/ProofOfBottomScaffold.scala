package ai.acyclic.prover.commons.jit.eval

object ProofOfBottomScaffold {

  sealed trait TupleThing {

    type Peer >: this.type <: TupleThing

    type Bottom <: Peer

    def proofOfBottom[TSub <: Peer](v: TSub): Bottom <:< TSub
  }

  case object Eye extends TupleThing {

    override type Peer = Eye.type

    override type Bottom = Eye.type

  }

  type ><:[+H, +T <: TupleThing] = Cons[? <: H, ? <: T]

  final case class Cons[H, T <: TupleThing](tail: T) extends TupleThing {

    override type Peer = ><:[H, T]

    override type Bottom = ><:[Nothing, tail.Bottom] & Peer

  }
}
