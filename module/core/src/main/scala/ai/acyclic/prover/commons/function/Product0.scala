package ai.acyclic.prover.commons.function

trait Product0 extends Product {

  final override def productIterator: Iterator[Nothing] = Iterator.empty
}
