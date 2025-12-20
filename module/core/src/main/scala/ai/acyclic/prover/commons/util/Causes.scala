package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.util.Causes.HasCauses

import scala.util.control.NoStackTrace

case class Causes[T <: Throwable](
    causes: Seq[T]
) extends HasCauses[T]
    with NoStackTrace {

  causes.drop(1).filter(_ != null).foreach(addSuppressed)

  override def getCause: T = causes.headOption.getOrElse(null.asInstanceOf[T])

  override def getMessage: String = s"[CAUSED BY ${causes.size} EXCEPTION(S)]"
}

object Causes {

  trait HasCauses[+T <: Throwable] extends Exception {

    def causes: Seq[T]

    def throwIfNonEmpty(): Unit = {

      if (causes.nonEmpty) throw this
    }
  }

  /**
    * Not a real throwable, just a placeholder indicating skipped execution
    */
  sealed trait Undefined extends Exception
  object Undefined extends Undefined

  /**
    * Wrap multiple causes as a single [[Causes]] exception, [[Undefined]] will be ignored
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

  /**
    * same as [[combine]], except that any [[Undefined]] in the input will cause the output to be also [[Undefined]]
    */
  def combineOrUndefined(causes: Seq[Throwable], foldUnary: Boolean = true): Throwable = {
    val undefined = causes.find(_.isInstanceOf[Undefined])
    undefined.getOrElse(
      Causes.combine(causes, foldUnary)
    )
  }
}
