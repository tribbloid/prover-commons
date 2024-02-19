package ai.acyclic.prover.commons.viz

import ai.acyclic.prover.commons.meta.ITyper
import ai.acyclic.prover.commons.refl.TypeIRMixin

package object format {

  type FormattedType = TypeIRMixin#TypeIR

  type TypeView = ITyper#TypeView
}
