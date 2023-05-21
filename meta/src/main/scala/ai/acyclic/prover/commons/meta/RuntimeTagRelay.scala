package ai.acyclic.prover.commons.meta

import scala.reflect.ClassTag

trait RuntimeTagRelay {

  import scala.reflect.runtime.universe

  implicit def classFromType[T](
      implicit
      ev: universe.TypeTag[T]
  ): ClassTag[T] = {

    val actualType = ev.tpe.dealias

    val classSymbols = actualType.baseClasses
    val firstClassSymbol = classSymbols.head.asInstanceOf[universe.ClassSymbol]

    lazy val classFromType = ev.mirror.runtimeClass(actualType)
    lazy val classFromSymbol = ev.mirror.runtimeClass(firstClassSymbol)

    val cTag: ClassTag[T] = ClassTag(classFromSymbol.asInstanceOf[Class[T]])

    cTag
  }
}

object RuntimeTagRelay {

  object Canonical extends RuntimeTagRelay
}
