package ai.acyclic.prover.commons.util

import scala.reflect.runtime.universe

object RuntimeClass {

  trait GetName {

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

  object Decoded extends GetName {

    override def normalise(enc: String): String = {
      val dec = universe.TypeName(enc).decodedName.toString
      val stripped = dec.slice(0, dec.indexOf("$"))
      stripped
    }
  }

  object Encoded extends GetName {

    override def normalise(enc: String): String = {
      enc
    }
  }

}
