val vs = versions()

val sparkV = vs.spark.v

dependencies {

    api(project(":prover-commons:core"))
    testFixturesApi(testFixtures(project(":prover-commons:core")))

    bothImpl("org.apache.spark:spark-core_${vs.scala.binaryV}:${sparkV}")
    bothImpl("org.apache.spark:spark-sql_${vs.scala.binaryV}:${sparkV}")
//    bothImpl("org.apache.spark:spark-mllib_${vs.scala.binaryV}:${sparkV}")
}