package ai.acyclic.prover.commons.util

import ai.acyclic.prover.commons.Delegating

import java.io.File
import scala.language.implicitConversions

object PathMagnet {

  trait Template {

    trait IK extends Delegating[String] {
      self: K =>

      def append(splitter: String)(part: String): K = { // TODO: should accept OptionMagnet[String]
        if (part == null || part.isEmpty) return this

        require(!part.startsWith(splitter), "cannot append a part that starts with the splitter")

        val str =
          if (unbox.endsWith(splitter)) unbox + part
          else unbox + splitter + part

        build(str)
      }

      def :/(part: String) = append("/")(part)
      def dot(part: String) = append(".")(part)

      override def toString: String = unbox
    }

    def separator: String

    def build(v: String): K
    def apply(vs: String*) = build(vs.mkString(separator))

    type K

    implicit def fromString(v: String): K = build(v)
  }

  // file system-agnostic, compatible with all common file systems, e.g. HDFS, NTFS and ext4
  object Universal extends Template {

    override def build(v: String): K = K(v) // fuck scala
    case class K(unbox: String) extends IK {

      def normaliseToLocal: LocalFS.K = {
        // always normalise / to local FS dependent FileSpeparator

        LocalFS.build {
          val v = unbox.replace('/', File.separatorChar)
          v
        }
      }
    }

    override def separator: String = "/"
  }

  object LocalFS extends Template {

    override def build(v: String): K = K(v) // fuck scala
    case class K(unbox: String) extends IK {

      def :\(part: String): K = append(File.separator)(part)

      lazy val glob_children: String = :\("*")
      lazy val glob_offspring: String = :\("**")

      def universal: Universal.K = Universal.build(unbox)
    }

    override def separator: String = File.separator
  }

  type UniversalPath = PathMagnet.Universal.K
  def UniversalPath = PathMagnet.Universal

  type LocalFSPath = PathMagnet.LocalFS.K
  def LocalFSPath = PathMagnet.LocalFS
}
