package com.tribbloids.graph.commons.util

trait TreeLike {

  def format: TreeFormat = TreeFormat.Indent2

  def nodeString: String // supports multiple lines

  def children: Seq[TreeLike] = Nil

  lazy val isLeaf: Boolean = children.isEmpty

//  final override def toString: String = treeString

  lazy val treeString: String = {

    val wText = format.wText(nodeString)

    if (isLeaf) {

      wText.prepend(format.LEAF).build
    } else {

      val selfT =
        if (isLeaf) wText.prepend(format.FORK)
        else wText.prepend(format.FORK, format.WRAP)

      val childrenTProtos = children.map { child =>
        format.wText(child.treeString)
      }

      val childrenTLast = childrenTProtos.lastOption.map { tt =>
        tt.prepend(format.DENOTE)
      }.toSeq

      val childrenTOthers = childrenTProtos.dropRight(1).map { tt =>
        tt.prepend(format.DENOTE, format.WRAP)
      }

      val result = (Seq(selfT) ++ childrenTOthers ++ childrenTLast)
        .map { v =>
          v.build
        }
        .mkString("\n")

      result
    }
  }
}

object TreeLike {

  case class Str(
      nodeString: String,
      override val children: Seq[Str] = Nil
  ) extends TreeLike {

    override val format: TreeFormat = TreeFormat.Indent2
  }
}
