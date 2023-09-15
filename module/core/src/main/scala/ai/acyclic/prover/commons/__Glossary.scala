package ai.acyclic.prover.commons

object __Glossary {

  /**
    * naming conventions:
    *
    * higher-kind arguments:
    *
    *   - this is the only place where single-character name is allowed, ambiguity can be minimised as they are only
    *     bind to current scope
    *   - arguments in a type projector (`type F[x] => List[x]`) can starts with lower-case.
    *
    * type or class names in a companion object:
    *
    *   - `Aux` for auxiliary type condition: introduced in shapeless as a shorthand definition for duck type
    *   - `AuxImpl` / `Impl` for extendable trait that satisfies `Aux`, Scala won't allow inheriting duck type,
    *     expecting heavy boilerplate
    *   - `Lt` for "less than" type condition: same as above, but for type refinement that is a subtype of `Aux`
    *   - `LtImpl` for extendable trait that satisfies `Lt`, similar to `AuxImpl`
    *   - `Gt` for "greater than" type condition, enough said
    *   - `Compat` for compatible type condition, instances that satisfy such condition can be implicitly converted to
    *     `Aux`, it could be an alias of `Lt`, `Gt` or `Aux` depending on situations
    *   - `Top` for supertype of all reifications of a generic type
    *   - `^` for the one and only implementation
    *
    * dependent type or class name suffixes:
    *
    *   - Should contains at least 2 characters to differentiate from higher-kind-arguments
    *   - `T` for type classes (can be omitted if it is a companion object)
    *   - `K` for generic type that has too many type parameters, only their much shorter aliases are supposed to be
    *     used
    *   - `Impl` for a type that can be extended by implementations classes, see `AuxImpl` / `LtImpl` / `GtImpl` for
    *     examples
    *   - `Lt`, same as `Lt` as a type name but can appear anywhere
    *   - `Compat` for compatible type, same as `Compat` as a type name but can appear anywhere
    *   - `Axiom` for axiom type that can be constructed arbitrarily, but only for compile-time verification and carry
    *     no runtime data. Consequently, they can be safely cast into each other (IF permitted at runtime, w/o
    *     triggering ClassCastException) or mixed into other classes, many of they have no constructor and all instances
    *     are casted from null/singleton, `shapeless.labelled.KeyTag` is a typical axiom type
    *     - the name "Axiom" refers to a convention by CHL correspondence (a.k.a. computational trinitarianism), an
    *       assumption with a "fake" constructive proof
    *     - "Phantom types" (types that will never be constructed/instantiated in runtime) are special cases of Axiom
    *       types, but not vice versa: The type system of Scala 3 removed type projection and instead relies heavily on
    *       dependent types, which make construction unavoidable in many logical reasoning
    *   - `Law` for `Axiom` type that also has a shortcut for runtime testing, most classes in `cats-law` are dedicated
    *     for this purpose
    *   - `_/\` for type upper bound
    *   - `_\/` for type lower bound
    *
    * type or class name prefixes:
    *
    *   - `_` (Inside a object) for a specific subtype that depends on this object
    *   - `__` for documentation object, an empty object indexed by IDE for documentation lookup
    *   - `Has` for classes that have an inner definition of the same name
    *
    * function name:
    *
    *   - `sanity` for simple sanity proof that can be conducted on Scala type system level, rely solely on Scala
    *     compiler to verify conjectures
    *
    * function name suffixes:
    *
    *   - `C` for computational effect, should only be called once and cached for the lifespan of the embedded object,
    *     definition should be `protected`
    */
}
