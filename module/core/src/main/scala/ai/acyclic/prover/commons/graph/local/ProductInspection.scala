package ai.acyclic.prover.commons.graph.local

import ai.acyclic.prover.commons.graph.local.DestructuringInspection.Destructured

import scala.reflect.ClassTag

object ProductInspection {

  def unapplyProduct[I <: Product](value: I, contents: Seq[Any]): Destructured[I] = {
    val contentSet = contents.toSet

    val inductions_contents: Seq[Either[I, Any]] = {

      val result: Seq[Either[I, Any]] = value.productIterator.to(LazyList).map { v =>
        if (contentSet.contains(v)) Right(v)
        else {
          v match {
            case sub: I =>
              Left(sub)

            case _ =>
              Right(v)
          }
        }
      }
      result
    }

    val induction: Seq[I] = inductions_contents.collect {
      case Left(v) => v
    }

    Destructured(
      value.productPrefix,
      induction,
      contents
    )
  }
}

abstract class ProductInspection[
    I <: Product, // inductively applicable
    T <: I // immediately applicable
](
    implicit
    tI: ClassTag[I]
) extends DestructuringInspection[I, T] {

  import DestructuringInspection.*

  def unapplyContents(value: I): Seq[Any] = Nil

  final override def unapplyAll(value: I): Destructured[I] = {

    val contents: Seq[Any] = unapplyContents(value)

    ProductInspection.unapplyProduct[I](value, contents)
  }
}
