package ai.acyclic.prover.commons.function.hom2

trait HasCircuit {

  object Circuit {

    trait Proto extends Domains {

      def apply(v: _I): _O[v.type]
    }

    type K1[-I] = Proto { type _I >: I }
    trait K1_[-I] extends Proto { type _I >: I }

    type K2[-I, +O] = Proto { type _I >: I; type _O[T] <: O }
    trait K2_[-I, +O] extends Proto { type _I >: I; type _O[T] <: O }

  }
}
