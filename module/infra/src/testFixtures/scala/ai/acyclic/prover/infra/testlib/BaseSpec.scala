package ai.acyclic.prover.infra.testlib

import org.scalatest.funspec.AnyFunSpec

import java.util.regex.Pattern
import scala.reflect.ClassTag

trait BaseSpec extends AnyFunSpec with TryCompile.Static.default.FromCodeMixin {

  import scala.reflect.runtime.universe.TypeTag

  /**
    * ScalaTest assertDoesNotCompile sometimes malfunction (due to unwashed execution order?) & doesn't perform literal
    * check if the code compiles successfully, the project compilation will fail
    */
  //  val shouldNotCompile: illTyped.type = illTyped

  // renamed to "shouldNotType"
  def shouldNotCompile(
      tryCompile: TryCompile,
      pattern: String = null
  ): Unit = {

    tryCompile match {

      case v: TryCompile.TypingError =>
        Option(pattern).foreach { pp =>
          val errorMsg = v.Error.filteredIssues.map(ii => ii.msg).mkString("\n").trim

          val _pp = Pattern.compile(pp, Pattern.DOTALL)
          val fit = _pp.matcher(errorMsg).matches()

          errorMsg.matches(pp)
          assert(
            fit,
            s"""
               |expecting type error with pattern:
               |$pp
               |
               |but get:
               |$errorMsg
               |""".stripMargin
          )
        }

      case v @ _ =>
        throw new AssertionError(
          s"""
             |expecting type error, but get:
             |$v
             |""".stripMargin.trim
        )
    }
  }

  def typeOfIt[T: TypeTag](
      subject: T
  )(fn: T => Unit): Unit = {

    val ttg: TypeTag[T] = implicitly[TypeTag[T]]
    it(ttg.tpe.toString) {
      fn(subject)
    }
  }

  def classOfIt[T: ClassTag](
      subject: T
  )(fn: T => Unit): Unit = {
    val ctg: ClassTag[T] = implicitly[ClassTag[T]]
    it(ctg.runtimeClass.toString) {
      fn(subject)
    }
  }
}

object BaseSpec {}
