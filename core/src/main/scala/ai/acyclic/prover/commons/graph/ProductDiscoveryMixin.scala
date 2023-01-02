package ai.acyclic.prover.commons.graph

import ai.acyclic.prover.commons.reflect.ScalaReflection
import ai.acyclic.prover.commons.{HasOuter, Padding, TextBlock}

// TODO: it should no longer serve as the backbone of ArityConjecture & ShapeConjecture runtime visualisation
//  which should be consistent with compile-time visualisation
trait ProductDiscoveryMixin extends Product {
  self: Semilattice.Upper.Node =>

  import ProductDiscoveryMixin._

  protected lazy val argList = this.productIterator.toList

  lazy val constructorString: String = {

    val hasOuter = this.getClass.getDeclaringClass != null

    if (hasOuter) {
      val list = HasOuter.outerListOf(this)

      val names = list.map { v =>
        val dec = decodedStrOf(v)

        dec
      }

      names.reverse.mkString(" ‣ ")
    } else {
      decodedStrOf(this)
    }

  }

  final override lazy val nodeText = {

    val notTree = this.argList
      .filterNot(v => v.isInstanceOf[Semilattice.Upper.Node])

    if (notTree.isEmpty) {

      constructorString
    } else {

      val _notTree = notTree.map { str =>
        TextBlock("" + str).padLeft(Padding.argLeftBracket).build
      }

      TextBlock(constructorString)
        .zipRight(
          TextBlock(_notTree.mkString("\n"))
        )
        .build
    }
  }

  final override lazy val outbound: Seq[Arrow.`~>`.Of[Semilattice.Upper.Node]] = {

    val result = this.argList.collect {
      case v: Semilattice.Upper.Node => v
    }
    result
  }
}

object ProductDiscoveryMixin {

  def decodedStrOf(v: AnyRef): String = {
    val clz = v.getClass
    val enc =
      clz.getCanonicalName.replace(clz.getPackage.getName, "").stripPrefix(".").stripSuffix("$")

    val dec = ScalaReflection.universe.TypeName(enc).decodedName
    dec.toString
  }
}
