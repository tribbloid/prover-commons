package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.debug.CallStackRef
import org.slf4j.LoggerFactory

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.control.ControlThrowable
import scala.util.{Failure, Success, Try}

/**
  * Created by peng on 18/09/16.
  */
object Retry {

  object DefaultRetry extends Retry

  val DEFAULT_RETRY: Int = 10
  val DEFAULT_DELAY: FiniteDuration = 0.millis

  object FixedInterval {

    def apply(
        maxRetries: Int = DEFAULT_RETRY,
        delay: FiniteDuration = DEFAULT_DELAY,
        silent: Boolean = false,
        callerStr: String = null
    ): Retry =
      Retry(
        maxRetries,
        { _ =>
          delay.toMillis
        },
        silent,
        callerStr
      )
  }

  object LinearBackoff {

    val MAX_RETRY_DELAY: FiniteDuration = DEFAULT_DELAY + 2.seconds

    def apply(
        maxRetries: Int = DEFAULT_RETRY,
        initialDelay: FiniteDuration = DEFAULT_DELAY,
        maxDelay: FiniteDuration = MAX_RETRY_DELAY
    ): Retry = {

      val retry = Retry(
        n = maxRetries,
        intervals = { nRemaining =>
          val attemptNum = maxRetries - nRemaining + 1
          val delayMs = (initialDelay.toMillis.toDouble * math.pow(2.0, attemptNum - 1)).toLong
          math.min(delayMs, maxDelay.toMillis)
        },
        silent = true
      )
      retry
    }
  }

  object ExponentialBackoff {

    def apply(
        maxRetries: Int = DEFAULT_RETRY,
        longestDelay: FiniteDuration = DEFAULT_DELAY,
        expBase: Double = 2.0,
        silent: Boolean = false,
        callerStr: String = null
    ): Retry = {
      Retry(
        maxRetries,
        { n =>
          (longestDelay.toMillis.toDouble.doubleValue() / Math.pow(expBase, n - 2)).asInstanceOf[Long]
        },
        silent,
        callerStr
      )
    }
  }

  class BypassingRule {

    def apply(e: Throwable): Bypassing = {
      e match {
        case ee: Bypassing => ee
        case _             => new Bypassing(e)
      }
    }

    class Bypassing(cause: Throwable) extends Throwable("Bypassing: " + this.getClass.getSimpleName, cause)

    def during[T](f: => T): T = {
      try {
        f
      } catch {
        case e: Exception => throw apply(e)
      }
    }
  }

  object BypassingRule {

    case object NoRetry extends BypassingRule
    case object Silent extends BypassingRule
  }

  case class RetryImpl[T](
      fn: () => T,
      retry: Retry = DefaultRetry
  ) {

    lazy val logger = LoggerFactory.getLogger(this.getClass)

    def get: T = _get(retry)

    @annotation.tailrec
    final protected def _get(
        retryOvrd: Retry
    ): T = {

      import retryOvrd.*

      // TODO: merge with CommonUtils
      lazy val _callerShowStr = {
        Option(showStr).getOrElse {
          CallStackRef
            .below(
              condition = _.isUnderClasses(classOf[Retry], classOf[RetryImpl[?]])
            )
            .showStr
        }
      }

      lazy val interval = intervals(n)
      Try(fn()) match {
        case Success(x) =>
          x
        case Failure(cc: ControlThrowable) =>
          throw cc // Instances of `Throwable` subclasses marked in this way should not normally be caught.
        case Failure(e: BypassingRule.NoRetry.Bypassing) =>
          throw e.getCause
        case Failure(e) if n > 1 =>
          if (!(silent || e.isInstanceOf[BypassingRule.Silent.Bypassing])) {
            logger.warn(
              s"Retrying locally on `${e.getClass.getSimpleName}` in ${interval.toDouble / 1000} second(s)... ${n - 1} time(s) left" +
                "\t@ " + _callerShowStr +
                "\n" + e.getClass.getCanonicalName + ": " + e.getMessage
            )
            logger.debug("\t\\-->", e)
          }
          Thread.sleep(interval)
          _get(retryOvrd.copy(n = n - 1))
        case Failure(e) =>
          logger.error(
            s"Retry failed after ${retry.n} attempts" +
              "\t@ " + _callerShowStr
          )
          throw e
      }
    }

    def map[T2](g: Try[T] => T2): RetryImpl[T2] = {

      val effectiveG: Try[T] => T2 = {
        case Failure(ee: BypassingRule.NoRetry.Bypassing) =>
          BypassingRule.NoRetry.during {
            g(Failure[T](ee.getCause))
          }
        case v =>
          g(v)
      }

      val result: RetryImpl[T2] = this.copy(() => effectiveG(Try(fn())))
      result
    }

    def mapSuccess[T2](g: T => T2): RetryImpl[T2] = {
      val effectiveG: Try[T] => T2 = {
        case Success(v)  => g(v)
        case Failure(ee) => throw ee
      }

      map(effectiveG)
    }
  }

}

case class Retry(
    n: Int = 3,
    intervals: Int => Long = { _ =>
      0L
    },
    silent: Boolean = false,
    showStr: String = null
) {

  import Retry.*

  def apply[T](fn: => T): T = {

    new RetryImpl[T](() => fn, this).get
  }

  def getImpl[T](fn: => T): RetryImpl[T] = {
    new RetryImpl[T](() => fn, this)
  }
}
