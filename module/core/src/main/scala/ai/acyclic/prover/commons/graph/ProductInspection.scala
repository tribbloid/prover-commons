package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.graph.local
import ai.acyclic.prover.commons.multiverse.CanUnapply

object ProductInspection extends ProductInspection {

  object Named extends ProductInspection {

    override lazy val inspect: Any => ProductInspection.Node_ = { v =>
      new Node(v) with Node.Named {}
    }
  }
}

trait ProductInspection extends UnapplyInspection {

  @transient override lazy val primary: CanUnapply[Any] = CanUnapply.Native.ForAny
  @transient override lazy val inlined: CanUnapply[Any] = CanUnapply.Bypass
}
