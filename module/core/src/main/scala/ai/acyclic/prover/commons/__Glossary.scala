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
    *   - `Conj` for conjecture type that are instantiated only for compile-time verification and carry no runtime data.
    *     Consequently, they have no constructor, can be instantiated from a singleton, and can be safely cast into each
    *     other or mixed into other classes, `shapeless.labelled.KeyTag` is a good example.
    *     - the name "conjecture" is a convention by Curry-Horward correspondence: referring to a type/assumption with a
    *       fake constructive proof on the fly
    *     - "Phantom types" (types that will never be instantiated in runtime) are special cases of Conjecture types,
    *       but not vice versa: The type system of Scala 3 removed type projection and instead relies heavily on
    *       dependent types, which make instantiation unavoidable in many logical reasoning.
    *   - `_/\` for type upper bound
    *   - `_\/` for type lower bound
    *
    * type or class name prefixes:
    *
    *   - `__` for documentation object, an empty object indexed by IDE for documentation lookup
    *   - `Has` for classes that have an inner definition of the same name
    *   - (Under a object) `The` for a specific subtype that depends on this object
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