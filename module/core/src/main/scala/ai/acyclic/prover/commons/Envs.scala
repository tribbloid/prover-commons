package ai.acyclic.prover.commons

import java.io.File

object Envs {

  case class Dir(protected val delegate: String) extends Delegating[String] {

    def append(splitter: String)(part: String): Dir = {
      require(!part.startsWith(splitter), "cannot append a part that starts with the splitter")

      if (delegate.endsWith(splitter)) Dir(delegate + part)
      else Dir(delegate + splitter + part)
    }

    def :/(part: String): Dir = append("/")(part)
    def :\(part: String): Dir = append(File.separator)(part)
    def dot(part: String): Dir = append(".")(part)

    override def toString: String = delegate
  }

  val USER_HOME: Dir = Dir(System.getProperty("user.home"))
  val USER_DIR: Dir = Dir(System.getProperty("user.dir"))

  val TEMP: String = "temp"

  val USER_TEMP_DIR: Dir = USER_DIR :\ TEMP
  val ROOT_TEMP_DIR: Dir = Dir(System.getProperty("java.io.tmpdir"))
}
