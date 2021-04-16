package com.tribbloids.graph.commons.util

import com.tribbloids.graph.commons.util.TextBlock.Padding
import com.tribbloids.graph.commons.util.reflect.{Reflection, ScalaReflection}

trait TreeLike {

  lazy val treeFormat: TreeFormat = TreeFormat.Indent2

  def nodeString: String // supports multiple lines

  def children: Seq[TreeLike] = Nil

  lazy val isLeaf: Boolean = children.isEmpty

  lazy val allOffsprings: Seq[TreeLike] = {
    val cc = children
    cc ++ children.flatMap { child =>
      child.allOffsprings
    }
  }

  def offsprings[T <: TreeLike](
      implicit
      ttag: Reflection.Runtime.TypeTag[T]
  ): Seq[T] = allOffsprings.collect {
    case v: T => v
  }

//  final override def toString: String = treeString

  lazy val treeString: String = {

    val wText = TextBlock(nodeString).indent(treeFormat.DOT)

    if (isLeaf) {

      wText.padLeft(treeFormat.LEAF).build

    } else {

      val selfT = wText.padLeft(treeFormat.FORK)

      val childrenTProtos: Seq[TextBlock] = children.map { child =>
        TextBlock(child.treeString)
      }

      val childrenTMid = childrenTProtos.dropRight(1).map { tt =>
        tt.padLeft(treeFormat.SUB)
      }

      val childrenTLast = childrenTProtos.lastOption.map { tt =>
        tt.padLeft(treeFormat.SUB_LAST)
      }.toSeq

      val result = (Seq(selfT) ++ childrenTMid ++ childrenTLast)
        .map { v =>
          v.build
        }
        .mkString("\n")

      result
    }
  }
}

object TreeLike {

  trait ProductAsTree extends TreeLike with Product {

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

  def decodedStrOf(v: AnyRef): String = {
    val clz = v.getClass
    val enc =
      clz.getCanonicalName.replaceAll(clz.getPackage.getName, "").stripPrefix(".").stripSuffix("$")

    val dec = ScalaReflection.universe.TypeName(enc).decodedName
    dec.toString
  }
}
