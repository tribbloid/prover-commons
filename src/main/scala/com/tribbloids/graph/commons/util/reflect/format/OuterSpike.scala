package com.tribbloids.graph.commons.util.reflect.format

import com.tribbloids.graph.commons.util.reflect.Reflection

trait Outer1 {
  self: OuterSpike =>

  case class Inner(v: Int) {

    //    val outer = self
  }
}

trait OuterSpike extends Outer1

object OuterSpike {

  {

    def cp(src: OuterSpike#Inner): OuterSpike#Inner = {
      src.copy()
    }

    def cp2[O <: OuterSpike](src: O#Inner): O#Inner = src.copy()

    val outer = new OuterSpike {
      val inner = this.Inner(123)
    }
    cp(outer.inner)
  }

  {

    // TODO: why it doesn't work?

    //    def cp(src: Reflection#TypeView): Reflection#TypeView = {
    //      src.copy()
    //    }

    //  def cp2[R <: Reflection](src: R#TypeView): R#TypeView = {
    //
    //    src.copy()
    //  }
  }

}
