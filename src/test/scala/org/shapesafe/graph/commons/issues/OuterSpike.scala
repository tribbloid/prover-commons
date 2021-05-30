package org.shapesafe.graph.commons.issues

trait OuterSpike {

  class Thing

  case class Inner(v: Thing) {

    //    val outer = self
  }
}

// TODO
//  https://stackoverflow.com/questions/67132770/in-scala-2-13-why-implicit-sometimes-can-be-summoned-directly-but-not-indirect
object OuterSpike {

//  {
//
//    def cp(src: OuterSpike#Inner): OuterSpike#Inner = {
//      src.copy()
//    }
//
//    def cp2[O <: OuterSpike](src: O#Inner): O#Inner = src.copy()
//
//    val outer = new OuterSpike {
//      val inner = this.Inner(new Thing)
//    }
//    cp(outer.inner)
//  }
}
