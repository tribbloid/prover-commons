package ai.acyclic.prover

package object commons {

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
    *   - `AuxEx` for a trait that satisfies `Aux`, Scala won't allow inheriting duck type, expecting redundant
    *     boilerplate
    *   - `Lt` for "less than" type condition: same as above, but for type refinement that is a subtype of `Aux`
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
    *   - `Kind` for generic type that has too many type parameters, only their much shorter aliases are supposed to be
    *     used
    *   - `Ex` for an extendable type, an alias of a trait with no refinement, `AuxEx` is one such example
    *
    * type or class name prefixes:
    *
    *   - `Has` for classes that have an inner definition of the same name
    *   - (In object) `This` for a specific subtype related to this object
    *
    * function name:
    *
    *   - `compileTimeCheck` for simple sanity proof that can be conducted on Scala type system level
    *
    * function name suffixes:
    *
    *   - `C` for computational effect, should only be called once and cached for the lifespan of the embedded object,
    *     definition should be `protected`
    */
}
