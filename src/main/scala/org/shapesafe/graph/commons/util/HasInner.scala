package org.shapesafe.graph.commons.util

trait HasInner {

  trait Inner extends HasOuter {

    final val outer: HasInner.this.type = HasInner.this

    type Self <: outer.Inner

    final val inner: Self = this.asInstanceOf[Self]
  }
}

object HasInner {}
