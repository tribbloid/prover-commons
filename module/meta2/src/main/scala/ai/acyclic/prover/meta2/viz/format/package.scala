package ai.acyclic.prover.meta2.viz

import ai.acyclic.prover.meta2.meta.ITyper
import ai.acyclic.prover.meta2.refl.TypeIRMixin

package object format {

  type FormattedType = TypeIRMixin#TypeIR

  type TypeView = ITyper#TypeView
}
