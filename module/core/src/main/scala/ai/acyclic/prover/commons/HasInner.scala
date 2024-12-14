package ai.acyclic.prover.commons

import ai.acyclic.prover.commons.multiverse.{CanEqual, View}

trait HasInner {

  import HasInner.*

  trait _Inner extends Inner {

    final override val outer: HasInner.this.type = HasInner.this
  }
}

object HasInner {

  trait Inner extends View.Equals {

    val outer: AnyRef

    @transient override def canEqualProjections =
      super.canEqualProjections :+ (CanEqual.Native.<~(outer))
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
