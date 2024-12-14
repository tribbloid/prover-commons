package ai.acyclic.prover.commons.same

object View {

  type Equals = CanEqual.Native.Equals

  object Equals {

    trait ByConstruction extends View.Equals {

      final protected val constructionID: java.util.UUID = java.util.UUID.randomUUID()

      @transient final override lazy val samenessKey: Any = {

        // only determine by definition ID if not a product
        this match {
          case v: Product => v.productIterator.toList
          case _          => constructionID
        }
      }
    }
  }
}
