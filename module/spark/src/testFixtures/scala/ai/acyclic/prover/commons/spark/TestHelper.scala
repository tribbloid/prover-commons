package ai.acyclic.prover.commons.spark

import ai.acyclic.prover.commons.util.Retry
import org.apache.hadoop.fs.FileUtil
import org.apache.spark.sql.{SQLContext, SparkSession}
import org.apache.spark.{SparkConf, SparkContext, SparkException}
import org.slf4j.LoggerFactory

import java.io.File
import java.util.{Date, Properties}
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object TestHelper {

  val properties: Properties = {

    val properties = new Properties()

    Try {
      properties.load(ClassLoader.getSystemResourceAsStream(".rootkey.csv"))
    }.recoverWith {
      case _: Exception =>
        Try {
          properties.load(ClassLoader.getSystemResourceAsStream("rootkey.csv"))
        }
    }.getOrElse {
      println("rootkey.csv is missing")
    }

    properties
  }

  def getProperty(key: String): Option[String] = {

    val result = Option(properties.getProperty(key))
    result match {
      case Some(v) =>
        println(s"[Test property] $key \t -> $v")
      case None =>
        println(s"[Test property] $key \t is undefined")
    }
    result

  }

  val S3Path: Option[String] = getProperty("S3Path")

  val AWSAccessKeyId: Option[String] = getProperty("AWSAccessKeyId")
  val AWSSecretKey: Option[String] = getProperty("AWSSecretKey")

  @transient lazy val SPARK_HOME: String = {

    System.getenv("SPARK_HOME")
  }

  @transient lazy val SPARK_SCALA_VERSION: String = {

    System.getenv("SPARK_SCALA_VERSION")
  }

  private case class SparkHomeEnv(home: String) {

    lazy val detectedScalaBinaryFromRelease: Option[String] = {
      val f = new File(home, "RELEASE")
      if (f.isFile) {
        val content = Try {
          val src = Source.fromFile(f, "UTF-8")
          try src.mkString
          finally src.close()
        }.toOption
        content.flatMap { s =>
          val r = "(?i)scala\\s+(2\\.\\d+)".r
          r.findFirstMatchIn(s).map(_.group(1))
        }
      } else None
    }

    lazy val detectedScalaBinaryFromJars: Option[String] = {
      val jarsDir = new File(home, "jars")
      if (jarsDir.isDirectory) {
        val files = Option(jarsDir.listFiles()).map(_.toSeq).getOrElse(Seq.empty)
        val patterns = Seq(
          "scala-library-(2\\.\\d+)\\..*\\.jar".r,
          "spark-core_(2\\.\\d+)-.*\\.jar".r,
          "spark-sql_(2\\.\\d+)-.*\\.jar".r,
          "spark-catalyst_(2\\.\\d+)-.*\\.jar".r,
          ".*_(2\\.\\d+)-.*\\.jar".r
        )
        files.view
          .map(_.getName)
          .flatMap(name => patterns.view.flatMap(p => p.findFirstMatchIn(name).map(_.group(1))))
          .headOption
      } else None
    }

    def detectScalaBinary: Option[String] = {
      detectedScalaBinaryFromRelease.orElse(detectedScalaBinaryFromJars)
    }
  }

  def validateSparkScalaBinaryConsistency(): Unit = {
    val homeOpt = Option(SPARK_HOME).filter(_.nonEmpty)
    homeOpt.foreach { home =>
      val detected = SparkHomeEnv(home).detectScalaBinary
      val envOpt = Option(SPARK_SCALA_VERSION).filter(_.nonEmpty)
      require(
        envOpt.nonEmpty,
        s"SPARK_SCALA_VERSION must be defined when SPARK_HOME is set (found SPARK_HOME='$home')." +
          detected.map(v => s" Expected binary Scala $v.").getOrElse("")
      )
      detected.foreach { bin =>
        val actual = envOpt.get
        require(
          actual == bin,
          s"SPARK_SCALA_VERSION='$actual' does not match Spark distribution at SPARK_HOME='$home' (binary Scala '$bin')."
        )
      }
    }
  }

  final val MAX_TOTAL_MEMORY = 8 * 1024
  final val MEMORY_PER_CORE = 1024
  //  final val EXECUTOR_JVM_MEMORY_OVERHEAD = 256 //TODO: remove, too complex

  final val MAX_CORES = MAX_TOTAL_MEMORY / MEMORY_PER_CORE

  @transient var sparkSessionInitialised: Boolean = false

  lazy val numCores: Int = {
    val cap = getProperty("MaxCores")
      .map(_.toInt)
      .getOrElse(MAX_CORES)
    var n = Math.min(
      Runtime.getRuntime.availableProcessors(),
      cap
    )

    if (n < 2) n = 2
    n
  }

  lazy val clusterSize_numCoresPerWorker_Opt: Option[(Int, Int)] = {

    val tuple = (
      getProperty("ClusterSize").map(_.toInt),
      getProperty("NumCoresPerWorker").map(_.toInt)
    )

    Option(SPARK_HOME) match {

      case Some(_) =>

        {
          validateSparkScalaBinaryConsistency()
        }

        tuple match {
          case (None, None) =>
            None
          case (Some(v1), Some(v2)) =>
            Some(v1 -> v2)
          case (Some(v1), None) =>
            Some(v1 -> Math.max(numCores / v1, 1))
          case (None, Some(v2)) =>
            Some(Math.max(numCores / v2, 1) -> v2)
        }
      case None =>
        tuple match {
          case (None, None) =>
          case _            =>
            LoggerFactory
              .getLogger(this.getClass)
              .warn("cannot use cluster mode as SPARK_HOME is missing")
        }
        None

    }
  }

  def clusterSizeOpt: Option[Int] = clusterSize_numCoresPerWorker_Opt.map(_._1)
  def numCoresPerWorkerOpt: Option[Int] = clusterSize_numCoresPerWorker_Opt.map(_._2)

  def maxFailures: Int = {
    getProperty("MaxFailures").map(_.toInt).getOrElse(4)
  }

  def numWorkers: Int = clusterSizeOpt.getOrElse(1)
  def numComputers: Int = clusterSizeOpt.map(_ + 1).getOrElse(1)

  def executorMemoryCapOpt: Option[Int] = clusterSizeOpt.map(clusterSize => MAX_TOTAL_MEMORY / clusterSize)

  def executorMemoryOpt: Option[Int] = for (n <- numCoresPerWorkerOpt; cap <- executorMemoryCapOpt) yield {
    Math.min(n * MEMORY_PER_CORE, cap)
  }

  case object CoreSettings {

    lazy val masterEnv: String = System.getenv("SPARK_MASTER")

    lazy val isLocal: Boolean = clusterSizeOpt.isEmpty || numCoresPerWorkerOpt.isEmpty

    lazy val masterStr: String = if (masterEnv != null) {
      masterEnv
    } else if (isLocal) {
      val masterStr = s"local[$numCores,$maxFailures]"
      println("initializing SparkContext in local mode:" + masterStr)
      masterStr
    } else {

      val masterStr =
        s"local-cluster[${clusterSizeOpt.get},${numCoresPerWorkerOpt.get},${executorMemoryOpt.get}]"
      println(s"initializing SparkContext in local-cluster simulation mode:" + masterStr)

      masterStr
    }

    /**
      * @return
      *   local mode: None -> local[n, 4] cluster simulation mode: Some(SPARK_HOME) -> local-cluster[m,n, mem]
      */
    lazy val asMap: Map[String, String] = {

      val base1 = Map(
        "spark.master" -> masterStr,
        "spark.task.maxFailures" -> maxFailures.toString
      )

      val base2 = if (CoreSettings.isLocal) {
        base1
      } else {
        base1 ++ Map(
          "spark.home" -> SPARK_HOME,
          //        "spark.executor.memory" -> (executorMemoryOpt.get + "m"),
          "spark.driver.extraClassPath" -> sys.props("java.class.path"),
          "spark.executor.extraClassPath" -> sys.props("java.class.path")
        )
      }

      base2 ++ Map(
        "spark.serializer" -> "org.apache.spark.serializer.KryoSerializer",
        "spark.kryoserializer.buffer.max" -> "512m",

        // TODO: kryo serializer is almost rendered unusable by https://github.com/EsotericSoftware/kryo/issues/885
        //  in addition, kryo serializer without class registration is not efficient
        //  this should be done automatically by an adaptive analyzer that collects required registration through runtime telemetry
        //  we may switch to Apache Fury in the future

        "spark.sql.warehouse.dir" -> Envs.WAREHOUSE_PATH,
        //      "hive.metastore.warehouse.dir" -> WAREHOUSE_PATH,
        "dummy.property" -> "dummy"
      )
    }
  }

  // if SPARK_PATH & ClusterSize in rootkey.csv is detected, use local-cluster simulation mode
  // otherwise use local mode
  lazy val TestSparkConf: SparkConf = {

    // always use KryoSerializer, it is less stable than Native Serializer
    val conf: SparkConf = new SparkConf(false)

    conf.setAll(CoreSettings.asMap)

    // TODO: remove, should be set by users
    //    Option(System.getProperty("fs.s3.awsAccessKeyId")).foreach { v =>
    //      conf.set("spark.hadoop.fs.s3.awsAccessKeyId", v)
    //      conf.set("spark.hadoop.fs.s3n.awsAccessKeyId", v)
    //      conf.set("spark.hadoop.fs.s3a.awsAccessKeyId", v)
    //    }
    //    Option(System.getProperty("fs.s3.awsSecretAccessKey")).foreach { v =>
    //      conf.set("spark.hadoop.fs.s3.awsSecretAccessKey", v)
    //      conf.set("spark.hadoop.fs.s3n.awsSecretAccessKey", v)
    //      conf.set("spark.hadoop.fs.s3a.awsSecretAccessKey", v)
    //    }

    conf.setAppName("Test")

    println(
      s"""
         |------------------------------------------
         |deriving SparkConf for testing ...
         |------------------------------------------
         |${conf.toDebugString}
         |==========================================
         |""".stripMargin
    )

    conf
  }

  lazy val TestSparkSession: SparkSession = {

    val builder = SparkSession
      .builder()
      .config(TestSparkConf)

    val hiveAvailable = Try {
      Class.forName("org.apache.spark.sql.hive.HiveSessionStateBuilder")
    }.isSuccess

    if (hiveAvailable) {
      builder.enableHiveSupport()
    }

    val session = builder.getOrCreate()
    val sc = session.sparkContext

    Retry.FixedInterval(3, 8000, silent = true).apply {
      // wait for all executors in local-cluster mode to be online
      require(
        sc.defaultParallelism == numCores, {
          val info = s"waiting for $numCores cores, only ${sc.defaultParallelism} are registered:\n" +
            getExecutorSummaryText(sc)
          LoggerFactory.getLogger(this.getClass).warn(info)
          "timeout: " + info
        }
      )
    }

    sparkSessionInitialised = true

    assureKryoSerializer(sc)
    session
  }

  def getExecutorSummaryText(sc: SparkContext): String = {
    val executors = sc.getExecutorMemoryStatus
    val executorReport = executors
      .map {
        case (k, v) =>
          s"$k :\t ${v._1} bytes available / ${v._2} bytes remaining"
      }
      .mkString("\n")

    s"""
       |Executors:
       |$executorReport
       |""".trim.stripMargin
  }

  lazy val TestSC: SparkContext = TestSparkSession.sparkContext
  lazy val TestSQL: SQLContext = TestSparkSession.sqlContext

  lazy val enableCheckpoint: Unit = {

    TestSC.setCheckpointDir("/tmp/spark-checkpoint/" + new Date().toString)
  }

  def setLoggerDuring[T](clazzes: Class[?]*)(fn: => T, level: String = "OFF"): T = {
    val logger_oldLevels = clazzes.map { clazz =>
      val logger = org.apache.log4j.Logger.getLogger(clazz)
      val oldLevel = logger.getLevel
      logger.setLevel(org.apache.log4j.Level.toLevel(level))
      logger -> oldLevel
    }
    try {
      fn
    } finally {
      logger_oldLevels.foreach {
        case (logger, oldLevel) =>
          logger.setLevel(oldLevel)
      }
    }
  }

  def assureKryoSerializer(sc: SparkContext, rigorous: Boolean = false): Unit = {
//    val ser = SparkEnv.get.serializer
//    require(ser.isInstanceOf[KryoSerializer])

    // will print a long warning message into stderr, disabled by default
    if (rigorous) {
      val rdd = sc.parallelize(Seq(sc)).map { v =>
        v.startTime
      }

      try {
        rdd.reduce(_ + _)
        throw new AssertionError("should throw SparkException")
      } catch {
        case e: SparkException =>
          val ee = e
          Predef.assert(
            ee.getMessage.contains("com.esotericsoftware.kryo.KryoException"),
            "should be triggered by KryoException, but the message doesn't indicate that:\n" + ee.getMessage
          )
        case e: Exception =>
          throw new AssertionError(s"Expecting SparkException, but ${e.getClass.getSimpleName} was thrown", e)
      }
    }
  }

  def intercept[EE <: Exception: ClassTag](fn: => Any): Unit = {
    val trial = Try {
      fn
    }
    val expectedErrorName = implicitly[ClassTag[EE]].runtimeClass.getSimpleName
    trial match {
      case Failure(_: EE) =>
      case Failure(e)     =>
        throw new AssertionError(s"Expecting $expectedErrorName, but get ${e.getClass.getSimpleName}", e)
      case Success(_) =>
        throw new AssertionError(s"expecting $expectedErrorName, but no exception was thrown")
    }
  }

  // TODO: clean up S3 as well
  // also, this should become a class that inherit cleanable
  def cleanTempDirs(paths: Seq[String] = Seq(Envs.USER_TEMP_DIR)): Unit = {
    paths.filter(_ != null).foreach { path =>
      try {
        val file = new File(path)
        Retry(3).apply {

          val absoluteFile = file.getAbsoluteFile
          if (absoluteFile != null && absoluteFile.exists())
            FileUtil.fullyDelete(absoluteFile, true)
        }

      } catch {
        case e: Throwable =>
          LoggerFactory.getLogger(this.getClass).warn("cannot clean tempDirs", e)
      }
    }
  }
}
