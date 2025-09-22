package ai.acyclic.prover.commons.spark

import org.apache.spark.rdd.RDD
import scala.language.implicitConversions

object RDDImplicits extends RDDImplicits

trait RDDImplicits {

  implicit def _rddView[T](self: RDD[T]): RDDView[T] = RDDView(self)
}
