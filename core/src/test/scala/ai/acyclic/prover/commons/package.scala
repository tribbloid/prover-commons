package ai.acyclic.prover

package object commons {

  /**
    * naming conventions:
    *
    * class name suffixes:
    *
    *   - `T` for type classes (can be omitted if it is a companion object)
    *   - `Kind` for generic type that has too many type parameters, only their much shorter aliases are supposed to be
    *     used
    *
    * function name suffixes:
    *
    *   - `C` for computational effect, should only be called once and cached for the lifespan of the embedded object,
    *     definition should be `protected`
    */
}
