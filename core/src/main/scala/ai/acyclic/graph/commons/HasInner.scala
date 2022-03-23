package ai.acyclic.graph.commons

trait HasInner {

  trait Inner extends HasOuter {

    final val outer: HasInner.this.type = HasInner.this

    type This <: outer.Inner

    final val inner: This = this.asInstanceOf[This]
  }
}

object HasInner {}
