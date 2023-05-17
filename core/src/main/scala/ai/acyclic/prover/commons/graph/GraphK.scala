package ai.acyclic.prover.commons.graph

trait GraphK[+L <: Law] extends Lawful.Construct[L] {

  type _E <: Engine
  def engine: _E

  type Dataset[+_]

  def entriesC: Dataset[NodeK.Compat[L, Value]]
}

object GraphK {

  type Aux[+L <: Law, V] = GraphK[L] { type Value = V }

  trait AuxT[+L <: Law, V] extends GraphK[L] {
    type Value = V
  }

  // Acronym of "Less Than"
  type Lt[+C <: Law, +A <: Arrow, +V] = Aux[C, _ <: V]

  // TODO: don't know how to define it in the new architecture`
//  trait Immutable[+Ops <: Node.Like[_, _]] extends GraphK[Ops] {
//
//    // the Memoization appears to confine GraphK to be only applicable to immutable graph
//    //  can this be lifted?
//    override lazy val ops: Value => Ops =
//      sameness
//        .Memoize[Value, Ops](
//          Ops
//        )
//  }

}
