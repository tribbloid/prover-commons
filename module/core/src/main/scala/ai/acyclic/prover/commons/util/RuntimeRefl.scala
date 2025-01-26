package ai.acyclic.prover.commons.util

import scala.reflect.runtime.universe

object RuntimeRefl {

  trait GetClassName {

    def simpleNameOf(v: Any): String = {
      v match {
        case p: Product =>
          val result = p.productPrefix
          result
        case _ =>
          val clz = v.getClass
          val name = clz.getSimpleName
          val cleanName =
            name.stripPrefix(".").stripSuffix("$")

          normalise(cleanName)
      }
    }

    def noPackageNameOf(v: Any): String = {

      val clz = v.getClass
      val name = clz.getCanonicalName
      val noPackage =
        name.replace(clz.getPackage.getName, "").stripPrefix(".").stripSuffix("$")

      normalise(noPackage)
    }

    def normalise(enc: String): String
  }

  object Decoded extends GetClassName {

    override def normalise(enc: String): String = {
      val dec = universe.TypeName(enc).decodedName.toString

      dec.indexOf("$") match {
        case -1 =>
          dec
        case v if v >= 0 =>
          val stripped = dec.slice(0, v)

          stripped

      }

    }
  }

  object Encoded extends GetClassName {

    override def normalise(enc: String): String = {
      enc
    }
  }

}
