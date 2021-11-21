package org.shapesafe.graph.commons.util

import scala.reflect.ClassTag

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

  def collectOffsprings[T <: TreeLike: ClassTag]: Seq[T] = allOffsprings.collect {
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

object TreeLike {}
