package ai.acyclic.prover.commons.spark

import ai.acyclic.prover.commons.spark.locality.PartitionIdPassthrough
import ai.acyclic.prover.commons.util.Caching.ConcurrentMap
import org.apache.spark.rdd.RDD
import org.apache.spark.sql._SQLHelper
import org.apache.spark.storage.{RDDInfo, StorageLevel}
import org.apache.spark.{HashPartitioner, SparkContext, TaskContext}

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Random

object RDDImplicits extends RDDImplicits

trait RDDImplicits {

  // (stageID -> threadID) -> isExecuted
  val perCoreMark: ConcurrentMap[(Int, Long), Boolean] = ConcurrentMap()
  // stageID -> isExecuted
  val perWorkerMark: ConcurrentMap[Int, Boolean] = ConcurrentMap()

  implicit class _rddImplicits[T](self: RDD[T]) {

    implicit lazy val rddClassTag: ClassTag[T] = _SQLHelper.rddClassTag(self)

    def sc: SparkContext = self.sparkContext

    def collectPerPartition: Array[List[T]] =
      self
        .mapPartitions(v => Iterator(v.toList))
        .collect()

    def multiPassMap[U: ClassTag](f: T => Option[U]): RDD[U] = {

      multiPassFlatMap(f.andThen(v => v.map(Iterable(_))))
    }

    // if the function returns None for it will be retried as many times as it takes to get rid of them.
    // core problem is optimization: how to SPILL properly and efficiently?
    // TODO: this is the first implementation, simple but may not the most efficient
    def multiPassFlatMap[U: ClassTag](f: T => Option[IterableOnce[U]]): RDD[U] = {

      val counter = sc.longAccumulator
      var halfDone: RDD[Either[T, IterableOnce[U]]] = self.map(v => Left(v))

      while (true) {
        counter.reset()

        val updated: RDD[Either[T, IterableOnce[U]]] = halfDone.map {
          case Left(src) =>
            f(src) match {
              case Some(res) => Right(res)
              case None =>
                counter add 1L
                Left(src)
            }
          case Right(res) => Right(res)
        }

        updated.persist().count()

        halfDone.unpersist()

        if (counter.value == 0) return updated.flatMap(_.toOption.get)

        halfDone = updated
      }
      sys.error("impossible")

    }

    def injectPassthroughPartitioner: RDD[(Int, T)] = {

      val withPID = self.map(v => TaskContext.get().partitionId() -> v)
      val result = withPID.partitionBy(new PartitionIdPassthrough(withPID.partitions.length))
      result
    }

    def storageInfoOpt: Option[RDDInfo] = {
      val infos = sc.getRDDStorageInfo
      infos.find(_.id == self.id)
    }

    def isPersisted: Boolean = {
      storageInfoOpt.forall(_.storageLevel != StorageLevel.NONE)
    }

    def assertIsBeacon(): Unit = {
      assert(isPersisted)
      assert(self.isEmpty())
    }

    def shufflePartitions: RDD[T] = {

      val randomKeyed: RDD[(Long, T)] = self.keyBy(_ => Random.nextLong())
      val shuffled = randomKeyed.partitionBy(new HashPartitioner(self.partitions.length))
      shuffled.values
    }

    def foreachExecute(): RDD[T] = {

      self.foreach(_ => ())
      self
    }

    /**
      * @param f
      *   function applied on each element
      * @tparam R
      *   result type
      * @return
      */
    def mapOncePerCore[R: ClassTag](f: T => R): RDD[R] = {

      self.mapPartitions { itr =>
        val stageID = TaskContext.get().stageId()
        //          val executorID = SparkEnv.get.executorId //this is useless as perCoreMark is a local singleton
        val threadID = Thread.currentThread().getId
        val allIDs = stageID -> threadID
        val alreadyRun = perCoreMark.synchronized {
          val alreadyRun = perCoreMark.getOrElseUpdate(allIDs, false)
          if (!alreadyRun) {
            perCoreMark.put(allIDs, true)
          }
          alreadyRun
        }
        if (!alreadyRun) {
          val result = f(itr.next())
          //            Thread.sleep(1000)
          Iterator(result)
        } else {
          Iterator.empty
        }
      }
    }

    def mapOncePerWorker[R: ClassTag](f: T => R): RDD[R] = {

      self.mapPartitions { itr =>
        val stageID = TaskContext.get().stageId()
        perWorkerMark.synchronized {
          val alreadyRun = perWorkerMark.getOrElseUpdate(stageID, false)

          val result = if (!alreadyRun) {
            val result = f(itr.next())
            //            Thread.sleep(1000)
            perWorkerMark.put(stageID, true)
            Iterator(result)
          } else {
            Iterator.empty
          }

          result
        }
      }
    }

  }
}
