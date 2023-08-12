package ai.acyclic.prover.commons.graph.law

abstract class Topology[L <: Law[_]](
    implicit
    arg: Law.ArgOf[L]
) extends Law.Arg {

  type _A = arg._A

  type Law_/\ = L
}
