package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.cap.Capabilities

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
    * type or class names in/of a companion object:
    *
    *   - `Aux` for auxiliary type condition: introduced in shapeless as a shorthand definition for duck type
    *   - `AuxImpl` / `Impl` for extendable trait that satisfies `Aux`, Scala won't allow inheriting duck type,
    *     expecting heavy boilerplate
    *   - `Lt` for "less than" type condition: same as above, but for type refinement that is a subtype of `Aux`
    *   - `LtImpl` for extendable trait that satisfies `Lt`, similar to `AuxImpl`
    *   - `Gt` for "greater than" type condition, enough said
    *   - `GtImpl` for extendable trait that satisfies `Gt`, similar to `AuxImpl`
    *   - `Compat` for compatible type condition, instances that satisfy such condition can be implicitly converted to
    *     `Aux`, it could be an alias of `Lt`, `Gt` or `Aux` depending on situations
    *   - `Top` for supertype of all reifications of a generic type
    *   - `^` for the one and only implementation
    *
    * type or class name suffixes:
    *
    *   - Should contains at least 2 characters to differentiate from higher-kind-arguments
    *   - `Like` (also `I` prefix, as in Java & C#) for closest interface that defines all members/methods of a class,
    *     usually used to bypass the limitation that only trait supports mixin & lazy initialisation
    *   - `T` for type classes (can be omitted if it is a companion object)
    *   - `K` for generic type that has too many type parameters, only their much shorter aliases are supposed to be
    *     used
    *   - `Impl` for a type that can be extended by implementations classes, see `AuxImpl` / `LtImpl` / `GtImpl` for
    *     examples
    *   - `Lt`, same as `Lt` as a type name but can appear anywhere
    *   - `Compat` for compatible type, same as `Compat` as a type name but can appear anywhere
    *   - `Cap`/`Tag` for capability tracking tag (see Scala 3 project Caprese for a definition of capability tracking),
    *     they are Mixin type of which the main type can be cast into to emulate reasoning of a substructural type
    *     system. E.g. definition `trait XXCap {self: CC =>}` allows any variable `val cc: CC` to be assigned to another
    *     variable `val ccWithCap = cc.asInstanceOf[CC with Has[XXCap]]`, or vice versa.
    *     - A typical example is `shapeless.labelled.KeyTag`
    *     - Several examples are also features in asynchronous computing library Kyo
    *     - see [[Capabilities]] for more explanation
    *   - `Axiom` for axiom type that can be constructed arbitrarily (assumed to have a constructive proof), but only
    *     for compile-time verification and carry no runtime data. Consequently, they can be safely cast into each other
    *     (IF permitted at runtime, w/o triggering ClassCastException) or mixed into other classes, many of they have no
    *     constructor and all instances are casted from null/singleton.
    *     - the name "Axiom" refers to a convention by CHL correspondence (a.k.a. computational trinitarianism), an
    *       assumption with a "fake" constructive proof
    *     - "Phantom types" (types that will never be constructed/instantiated in runtime) are special cases of Axiom
    *       types, but not vice versa: The type system of Scala 3 removed type projection and instead relies heavily on
    *       dependent types, which make construction unavoidable in many logical reasoning
    *   - `Law` for `Axiom` type that also has a shortcut for runtime testing, most classes in `cats-law` are dedicated
    *     for this purpose
    *   - `ImpX` (where X is an integer) for low-priority implicit definitions, Scala compiler will throw an error when
    *     multiple implicit functions/values with same priority can be summoned, in this case they need to be spread
    *     into a hierarchy of traits (e.g. `object Ops extends OpsImp0`, `OpsImp0 extends OPsImp1` ...) to have
    *     different priorities, see [[__ImplicitSearchOrder]] for the rule of implicit priority
    *   - `_/\` for type upper bound
    *   - `_\/` for type lower bound
    *   - `Magnet` for types armed with native implicit conversions that automatically coerce other types into it when
    *     necessary
    *   - `Ops`/`Ext` for type extensions in Scala 3, or wrapper types that mimic this feature, in later case, implicit
    *     functions are usually defined in the associated scope of such type (see [[__ImplicitSearchOrder]] for an
    *     explanation of the associated scope), but not always.
    *   - `View`/`Wrapper` is a special kind of `Ops`/`Ext` classes that also contains custom members/methods for
    *     resource allocation, serialisation, equality, or other constructs that affects its runtime behaviour. For
    *     historical reasons, a `Ops`/`Ext` class can also be named `View`
    *   - `Case`/`TypeCase` for type class (as in Haskell terminology, this name should be avoided in code, as it is not
    *     a JVM class in OOP), and object/term that can bind (be summoned from) type(s) and nothing else, required by
    *     any type system that contains system F
    *   - `Factory` for a class that can be used to construct instances of another class, usually defined in a companion
    *   - `Giver` for an implicit `Factory` of which cases can be summoned for a type, usually defined as a companion
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
    *
    * comments:
    *   - `TODO` for future work
    *   - `FIXME` for part that may have room for further optimisations
    */
}
