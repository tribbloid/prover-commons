package org.shapesafe.graph.commons.util

trait SingletonSummoner {

  implicit def summonSingleton[T <: this.type with SingletonSummoner]: T = this.asInstanceOf[T]
}
