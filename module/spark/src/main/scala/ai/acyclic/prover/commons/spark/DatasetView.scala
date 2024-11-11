package ai.acyclic.prover.commons.spark

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Dataset
import org.apache.spark.storage.StorageLevel

import scala.collection.Map
import scala.collection.immutable.ListMap
import scala.language.implicitConversions

/**
  * Created by peng on 02/02/17.
  */
case class DatasetView[T](self: Dataset[T]) {

  def checkpointImpl(eager: Boolean = true): Dataset[T] = {
    self.persist(StorageLevel.DISK_ONLY)
    val result =
      try {
        self.checkpoint(eager)
      } finally {
        self.unpersist()
      }
    result
  }

  def toMapRDD(keepNull: Boolean = false): RDD[Map[String, Any]] = {
    val headers = self.schema.fieldNames

    val result: RDD[Map[String, Any]] = self.toDF().rdd.map { row =>
      ListMap(headers.zip(row.toSeq) *)
    }

    val filtered =
      if (keepNull) result
      else
        result.map { map =>
          map.filter(_._2 != null)
        }

    filtered
  }
}

object DatasetView {

  trait Implicits {

    implicit def fromDataset[T](self: Dataset[T]): DatasetView[T] = DatasetView(self)
  }

  object Implicits extends Implicits

}
