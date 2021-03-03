package com.tribbloids.graph.commons.util

trait TreeLike {

  lazy val format: TreeFormat = TreeFormat.Indent2

  def nodeString: String // supports multiple lines

  def children: Seq[TreeLike] = Nil

  lazy val isLeaf: Boolean = children.isEmpty

//  final override def toString: String = treeString

  lazy val treeString: String = {

    val wText = format.wText(nodeString).indent(format.DOT)

    if (isLeaf) {

      wText.prepend(format.LEAF).build

    } else {

      val selfT = wText.prepend(format.FORK)

      val childrenTProtos: Seq[format.WText] = children.map { child =>
        format.wText(child.treeString)
      }

      val childrenTMid = childrenTProtos.dropRight(1).map { tt =>
        tt.prepend(format.SUB)
      }

      val childrenTLast = childrenTProtos.lastOption.map { tt =>
        tt.prepend(format.SUB_LAST)
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

object TreeLike {}
