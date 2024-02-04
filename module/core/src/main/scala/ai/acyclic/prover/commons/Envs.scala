package ai.acyclic.prover.commons

import java.io.File
import scala.language.implicitConversions

object Envs {

  case class Dir(parts: String) {

    def append(splitter: String)(part: String): Dir = {
      require(!part.startsWith(splitter), "cannot append a part that starts with the splitter")

      if (parts.endsWith(splitter)) Dir(parts + part)
      else Dir(parts + splitter + part)
    }

    def :/(part: String): Dir = append("/")(part)
    def :\(part: String): Dir = append(File.separatorChar.toString)(part)
    def dot(part: String): Dir = append(".")(part)

    override def toString: String = parts
  }

  object Dir {

    implicit def unbox(dir: Dir): String = dir.parts
  }

  val USER_HOME: Dir = Dir(System.getProperty("user.home"))
  val USER_DIR: Dir = Dir(System.getProperty("user.dir"))

  val TEMP: String = "temp"

  val USER_TEMP_DIR: Dir = USER_DIR :\ TEMP
  val ROOT_TEMP_DIR: Dir = Dir(System.getProperty("java.io.tmpdir"))
}
