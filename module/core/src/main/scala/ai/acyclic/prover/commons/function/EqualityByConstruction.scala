package ai.acyclic.prover.commons.function

import ai.acyclic.prover.commons.same.EqualBy

trait EqualityByConstruction extends EqualBy {

  final protected val constructionID: java.util.UUID = java.util.UUID.randomUUID()

  final override lazy val samenessKey: Any = {

    // only determine by definition ID if not a product
    this match {
      case v: Product => v.productIterator.toList
      case _          => constructionID
    }
  }
}
