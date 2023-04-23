package ai.acyclic.prover

package object commons {

  /**
    * naming conventions:
    *
    * type or class names in a companion object:
    *
    *   - `Aux` for auxiliary type refinement: introduced in shapeless as a shorthand definition for duck type
    *   - `AuxT` for a trait that satisfies `Aux`, Scala won't allow inheriting duck type, expecting redundant
    *     boilerplate
    *   - `Lt` for "less than" type refinement: same as above, but for type refinement that is a subtype of `Aux`
    *   - `Gt` for "greater than" type refinement, enough said
    *   - `Top` for supertype of all reifications of a generic type
    *   - `^` for the one and only implementation
    *
    * type or class name suffixes:
    *
    *   - `T` for type classes (can be omitted if it is a companion object)
    *   - `Kind` for generic type that has too many type parameters, only their much shorter aliases are supposed to be
    *     used
    *
    * type or class name prefixes:
    *
    *   - `Has` for classes that have an inner definition of the same name
    *   - (In object) `This` for a specific subtype related to this object
    *
    * function name suffixes:
    *
    *   - `C` for computational effect, should only be called once and cached for the lifespan of the embedded object,
    *     definition should be `protected`
    */
}
