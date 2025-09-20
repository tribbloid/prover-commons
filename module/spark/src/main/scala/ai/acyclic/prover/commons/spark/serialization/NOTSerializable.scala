package ai.acyclic.prover.commons.spark.serialization

import BeforeAndAfterShipping.Trigger

import java.io.NotSerializableException

/**
  * Any subclass in the closure cleaned by Spark ClosureCleaner will trigger a runtime error.
  */
trait NOTSerializable extends BeforeAndAfterShipping {

  import NOTSerializable.*

  {
    trigger
  }

  private lazy val trigger = {
    val msg = s"${this.getClass.getCanonicalName} is NOT serializable"
    Trigger(Internal(msg))
  }
}

object NOTSerializable {

  case class Internal(msg: String) extends BeforeAndAfterShipping {

    private lazy val error =
      new NotSerializableException(msg)

    override def beforeDeparture(): Unit = {
      throw error
    }

    override def afterArrival(): Unit = {
      throw error
    }
  }
}
