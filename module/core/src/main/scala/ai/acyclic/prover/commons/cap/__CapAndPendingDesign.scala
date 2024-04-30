package ai.acyclic.prover.commons.cap

object __CapAndPendingDesign {

  /**
    * minimally viable features:
    *
    *   - Capability ([[ai.acyclic.prover.commons.cap.Capability.>>]]) and Pending Effect (Pending,
    *     [[ai.acyclic.prover.commons.cap.Pending.<<]]) are compile-time only mixin that are erased at runtime
    *   - variables with types constructed from these mixins are tracked in their scope
    *   - Cap/Pending can always be added (mixin) or revoked (demixin) explicitly in program
    *     - TODO: is it possible to demixin without knowing the full type? either as function return type or flatMap arg
    *     - this is critical in writing an effect that only revokes the necessary pending
    *     - need an experiment to figure it out
    *   - The original type with no Cap/Pending can be fully recovered from the type with Cap/Pending
    *     - consequently, none of them can be a subtype mix-in directly (`with` / `&` keyword, union type mixin like
    *       `or` / `|` is fine tho), they can only be declared as type parameters
    *   - Cap can be revoked by upcasting/coercing with no effect
    *   - Pending can be added by upcasting/coercing with no effect, and may be removed implicitly with SOME effects
    *     (e.g. checking null pointer)
    *   - both Cap and Pending can be implicitly coerced into something conventional
    *   - sequence of mixin doesn't matter, they can only define a semi-lattice
    *
    * typical Caps are:
    *
    * typical Pending Effects are:
    *
    *   - OrNull, can be revoked by checking null pointer, can be coerced into [[Option[T]`]], can use map or flatMap
    *   - Aborts[-E], can be revoked using an abort handler (`CanThrow` in Scala 3.4) in scope, can be coerced into
    *     [[Either[E, T]]] or [[scala.util.Try[T]]], can use map or flatMap
    *   - Capture (as in Scala 3.4 capture checking / Caprese, should not be confused with Capability) is a special kind
    *     of Pending Effect that only takes variable type(s), new variable can be added by upcasting with no effect
    */
}
