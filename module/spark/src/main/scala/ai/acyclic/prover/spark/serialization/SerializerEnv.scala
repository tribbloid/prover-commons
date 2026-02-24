package ai.acyclic.prover.spark.serialization

import ai.acyclic.prover.commons.jit.Hom.Fn
import ai.acyclic.prover.commons.jit.eval.Args
import Args.{><:, T0}
import org.apache.spark.SparkConf
import org.apache.spark.serializer.{JavaSerializer, KryoSerializer, Serializer, SerializerInstance}

object SerializerEnv {

  case class Ops(conf: SparkConf) {

    import org.apache.spark.sql.catalyst.ScalaReflection.universe.*

    @transient lazy val _conf: SparkConf = conf
      .registerKryoClasses(Array(classOf[TypeTag[?]]))

    @transient lazy val javaSerializer: JavaSerializer = new JavaSerializer(_conf)
    @transient lazy val javaOverride: () => Some[SerializerInstance] = { // TODO: use singleton?
      () =>
        Some(javaSerializer.newInstance())
    }

    @transient lazy val kryoSerializer: KryoSerializer = new KryoSerializer(_conf)
    @transient lazy val kryoOverride: () => Some[SerializerInstance] = { // TODO: use singleton?
      () =>
        Some(kryoSerializer.newInstance())
    }

    @transient lazy val allSerializers: List[Serializer] = List(javaSerializer, kryoSerializer)
  }

  lazy val apply: Fn.CachedImpl[SparkConf ><: T0, Ops] = Fn
    .at { (v: SparkConf) =>
      Ops(v)
    }
    .cached()

  lazy val Default: Ops = apply(new SparkConf())
}
