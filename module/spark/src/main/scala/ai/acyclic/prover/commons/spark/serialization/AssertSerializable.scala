package ai.acyclic.prover.commons.spark.serialization

import ai.acyclic.prover.commons.spark.serialization.AssertSerializable.strongCondition
import ai.acyclic.prover.commons.util.Causes
import org.apache.spark.serializer.Serializer

import java.nio.ByteBuffer
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

case class AssertSerializable[T <: Any](
    element: T,
    serializers: Seq[Serializer] = SerializerEnv.Default.allSerializers
) {

  @transient implicit lazy val ctg: ClassTag[T] = ClassTag(element.getClass)

  def on(
      condition: (T, T) => Any
  ): Unit = {

    val tries: Seq[Try[Any]] = serializers.map { ser =>
      Try {
        val serInstance = ser.newInstance()
        val binary: ByteBuffer = serInstance.serialize(element)
        assert(binary.array().length >= 1)
        val element2 = serInstance.deserialize[T](binary)
        //      assert(!element.eq(element2))
        condition(element, element2)
      }
        .recover {
          case e: Throwable =>
            throw new AssertionError(
              s"cannot serialize with ${ser.getClass.getSimpleName}: ${e.getMessage}",
              e
            )
        }
    }

    val errors = tries.collect {
      case Failure(ee) => ee
    }

    if (errors.nonEmpty) throw new Causes(errors)
  }

  def weakly(): Unit = {
    on((_, _) => true)
  }

  def strongly(): Unit = {

    on((a, b) => strongCondition(a, b))
  }
}

object AssertSerializable {

  def strongCondition[T <: Any]: (T, T) => Any = { (v1: T, v2: T) =>
    assert(v1.hashCode() == v2.hashCode(), s"hash code after deserialization is different: $v1 != $v2")
    assert(v1.toString == v2.toString, s"value.toString after deserialization is different: $v1 != $v2")
    assert(
      (v1: T) equals (v2: T),
      s"value after deserialization is different: $v1 != $v2"
    )
    //    (v1, v2) match {
    //      case (_1: AnyRef, _2: AnyRef) =>
    //        if (!v1.getClass.getName.endsWith("$")) {
    //        TODO: remove, kryo serializer automatically interns https://stackoverflow.com/questions/10578984/what-is-java-string-interning
    //
    //          assert(!(_1 eq _2))
    //        }
    //    }
  }
}
