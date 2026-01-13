package ai.acyclic.prover.commons.jit

trait Product0 extends Product {

  final override def productIterator: Iterator[Nothing] = Iterator.empty
}
