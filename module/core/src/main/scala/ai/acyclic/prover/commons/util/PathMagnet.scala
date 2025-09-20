package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.cap.Capability
import ai.acyclic.prover.commons.cap.Capability.<>

import java.io.File
import scala.language.implicitConversions

object PathMagnet {

  trait Template {

    def separator: String

    object Cap extends Capability
    type Cap = Cap.type

    type K = String <> Cap

    implicit final def build(v: String): K = Capability(v) <> Cap
    final def apply(vs: String*): K = build(vs.mkString(separator))

    implicit class _ext0(self: K) {

      def append(splitter: String)(part: String): K = { // TODO: should accept OptionMagnet[String]
        if (part == null || part.isEmpty) return self

        require(
          !part.startsWith(splitter), {
            s"cannot append a part `${part}` that starts with the splitter"
          }
        )

        val str =
          if (self.endsWith(splitter)) self + part
          else self + splitter + part

        build(str)
      }

      def :/(part: String): K = append("/")(part)
      def dot(part: String): K = append(".")(part)

      def dotIfNoExistingExtension(part: String): K = {

        if (self.lastIndexOf(".") < self.lastIndexOf(File.separator)) {
          dot(part)
        } else {
          self
        }
      }
    }
  }

  // file system-agnostic, compatible with all common file systems, e.g. HDFS, NTFS and ext4
  object URI extends Template {

    override def separator: String = "/"

    implicit class _ext(unbox: K) {

      def normaliseToLocal: LocalFS.K = {
        // always normalise / to local FS dependent FileSpeparator

        LocalFS.build {
          val v = unbox.replace('/', File.separatorChar)
          v
        }
      }
    }

  }

  object LocalFS extends Template {

    override def separator: String = File.separator

    implicit class _ext(unbox: K) {

      def \\(part: String): K = unbox.append(File.separator)(part) // named such as :\ is already taken

      lazy val glob_children: K = \\("*")
      lazy val glob_offspring: K = \\("**")

      def universal: URI.K = URI.build(unbox)
    }
  }

  type URIPath = PathMagnet.URI.K
  def URIPath: URI.type = PathMagnet.URI

  type LocalFSPath = PathMagnet.LocalFS.K
  def LocalFSPath: LocalFS.type = PathMagnet.LocalFS
}
