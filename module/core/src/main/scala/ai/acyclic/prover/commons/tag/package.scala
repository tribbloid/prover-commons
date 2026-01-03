package ai.acyclic.prover.commons

package object tag {

  /**
    * A type system hack to mixin Tag type into an object after its creation
    *
    * `(x: C).asInstanceOf[C TaggedBy X]` can introduce the implicit scope of X to `x` on the fly, this can mulate
    * reasoning of a * * substructural type system
    *
    * In Scala 2, type arguments are erased, intersection types are mostly erased except the first argument. Henceforth,
    * runtime typecast from C to (C TaggedBy X) or (C TaggedBy X TaggedBy Y) won't throw any exception.
    *
    * Not sure about Scala 3 yet.
    *
    *   - This is not the only infeasible implementation, similar pattern is used in other libraries:
    *     - a typical example is `shapeless.labelled.KeyTag`
    *     - Several examples are also features in asynchronous computing library Kyo
    */
  trait Tagged[+C <: Tag]

  infix type TaggedBy[+T, +C <: Tag] = T & Tagged[C]

  {
    // sanity, wow it actually works on Scala 2
    type K = Int TaggedBy Tag
  }

  infix type <>[+T, +C <: Tag] = TaggedBy[T, C] // left associated
  // CAUTION: don't use right associated operator, it is the source of mandated parentheses

  trait Pending[-C <: Tag] // TODO: implement this
}
