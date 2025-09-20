package ai.acyclic.prover.commons

object __AxiomaticPrinciples {

  /**
    * principles (CHL correspondence):
    *
    *   - program = proof, type = proposition/axiom/theorem/lemma/corollary, term = expression
    *   - if a theorem is longer than its proof, something is very wrong
    *   - the most important interface in axiomatic programming is type, unlike other forms of programming
    *   - thus, it is important to ensure that for each theorem, the input & output types can be expressed as short as
    *     possible, there are 2 methods:
    *     - in the theorem, declare an alias of the output type, using input types as type parameters
    *     - declare the output type as a dependent type of an existing term
    *   - a chained given definition (an implicit function/class that has using argument(s)) that is not a theorem
    *     (doesn't provide any new type to be used by other theorems as a lemma ) represents a leaky abstraction, it
    *     cannot be summoned without a (usually extremely verbose) declaration of its lemma, AVOID IT AT ALL COSTS!
    *   - implicit class (as a manifestation of Scala 3 extension) is preferred over implicit conversion
    */
}
