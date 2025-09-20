package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.util.Causes.HasCauses

import scala.util.control.NoStackTrace

case class Causes[T <: Throwable](
    causes: Seq[T]
) extends HasCauses[T]
    with NoStackTrace {

  override def getCause(): T = causes.headOption.getOrElse(null.asInstanceOf[T])

  override def getMessage(): String = s"[CAUSED BY ${causes.size} EXCEPTION(S)]"
}

object Causes {

  trait HasCauses[+T <: Throwable] extends Exception {

    def causes: Seq[T]

    def throwIfNonEmpty(): Unit = {

      if (causes.nonEmpty) throw this
    }
  }

  /**
    * Not a real throwable, just a placeholder indicating lack of trials
    */
  sealed trait Undefined extends Exception
  object Undefined extends Undefined

  /**
    * @param foldUnary
    *   not recommended to set to false, should use Wrapper() directly for type safety
    * @return
    */
  def combine[T <: Throwable](causes: Seq[T], foldUnary: Boolean = true): Throwable = {
    val _causes = causes.distinct.filterNot(_.isInstanceOf[Undefined])
    if (_causes.isEmpty) return Undefined

    if (_causes.size == 1 && foldUnary) {
      _causes.head
    } else {
      Causes(causes = _causes)
    }
  }
}
