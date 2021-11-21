package org.shapesafe.graph.commons.util

import org.shapesafe.graph.commons.util.reflect.ScalaReflection

// TODO: it should no longer serve as the backbone of ArityConjecture & ShapeConjecture runtime visualisation
//  which should be consistent with compile-time visualisation
trait ProductTree extends TreeLike with Product {

  import ProductTree._

  private lazy val argList = this.productIterator.toList

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

  final override lazy val nodeString = {

    val notTree = this.argList
      .filterNot(v => v.isInstanceOf[TreeLike])

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

  final override lazy val children: List[TreeLike] = {

    this.argList.collect {
      case v: TreeLike => v
    }
  }
}

object ProductTree {

  def decodedStrOf(v: AnyRef): String = {
    val clz = v.getClass
    val enc =
      clz.getCanonicalName.replace(clz.getPackage.getName, "").stripPrefix(".").stripSuffix("$")

    val dec = ScalaReflection.universe.TypeName(enc).decodedName
    dec.toString
  }
}
