package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.multiverse.{CanEqual, Projection}

trait HasInner {

  import HasInner.*

  trait _Inner extends Inner {

    final override val outer: HasInner.this.type = HasInner.this
  }
}

object HasInner {

  trait Inner extends Projection.Equals {

    val outer: AnyRef

    {
      canEqualProjections += CanEqual.Native.on(outer)
    }
  }

  object Inner {}

  def outerListOf(v: Any): List[Any] = {

    val self = List(v)

    val outers = v match {
      case vv: Inner =>
        val outer = vv.outer
        outerListOf(outer)
      case _ =>
        Nil
    }

    self ++ outers
  }
}
