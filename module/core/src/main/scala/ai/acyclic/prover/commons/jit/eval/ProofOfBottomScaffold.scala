package ai.acyclic.prover.commons.jit.eval

object ProofOfBottomScaffold {

  private def uncheckedProof[A, B]: A <:< B =
    scala.Predef.$conforms[A].asInstanceOf[A <:< B]

  sealed trait TupleThing {

    type Peer >: this.type <: TupleThing

    type Bottom <: Peer

    def proofOfBottom[TSub <: Peer]: Bottom <:< TSub
  }

  case object Eye extends TupleThing {

    override type Peer = Eye.type

    override type Bottom = Eye.type

    override def proofOfBottom[TSub <: Peer]: Bottom <:< TSub = uncheckedProof
  }

  type ><:[+H, +T <: TupleThing] = Cons[? <: H, ? <: T]

  final case class Cons[H, T <: TupleThing](tail: T) extends TupleThing {

    override type Peer = ><:[H, T]

    override type Bottom = ><:[Nothing, tail.Bottom] & Peer

    override def proofOfBottom[TSub <: Peer]: Bottom <:< TSub = {
      (tail: TupleThing) match {
        case Eye =>
          ()
        case cons: Cons[_, _] =>
          cons.proofOfBottom[cons.Peer]
      }

      uncheckedProof[Bottom, TSub]
    }
  }
}
