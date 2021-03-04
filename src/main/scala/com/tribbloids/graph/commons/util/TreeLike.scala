package com.tribbloids.graph.commons.util

import com.tribbloids.graph.commons.util.TextBlock.Padding

trait TreeLike {

  lazy val format: TreeFormat = TreeFormat.Indent2

  def nodeString: String // supports multiple lines

  def children: Seq[TreeLike] = Nil

  lazy val isLeaf: Boolean = children.isEmpty

//  final override def toString: String = treeString

  lazy val treeString: String = {

    val wText = TextBlock(nodeString).indent(format.DOT)

    if (isLeaf) {

      wText.padLeft(format.LEAF).build

    } else {

      val selfT = wText.padLeft(format.FORK)

      val childrenTProtos: Seq[TextBlock] = children.map { child =>
        TextBlock(child.treeString)
      }

      val childrenTMid = childrenTProtos.dropRight(1).map { tt =>
        tt.padLeft(format.SUB)
      }

      val childrenTLast = childrenTProtos.lastOption.map { tt =>
        tt.padLeft(format.SUB_LAST)
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

  trait ProductMixin extends TreeLike with Product {

    private val argList = this.productIterator.toList

    private val constructorName = {

      val clz = this.getClass

      val result = clz.getCanonicalName.replaceAllLiterally(clz.getPackage.getName, "").stripPrefix(".")
      result
    }

    final override lazy val nodeString = {

      val notTree = this.argList
        .filterNot(v => v.isInstanceOf[TreeLike])

      if (notTree.isEmpty) {

        constructorName
      } else {

        val _notTree = notTree.map { str =>
          TextBlock(str.toString).padLeft(Padding.argLeftBracket).build
        }

        val fill = Padding(
          constructorName,
          constructorName.map(v => ' ')
        )

        TextBlock(_notTree.mkString("\n")).padLeft(fill).build
      }
    }

    final override lazy val children: List[TreeLike] = {

      this.argList.collect {
        case v: TreeLike => v
      }
    }
  }
}
