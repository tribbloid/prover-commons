val vs = versions()

dependencies {
//scala compiler
    api("org.scala-lang:scala-compiler:${vs.scala.v}")

    // https://mvnrepository.com/artifact/com.lihaoyi/sourcecode
    api("com.lihaoyi:sourcecode_${vs.scala.artifactSuffix}:0.4.4")

    // https://mvnrepository.com/artifact/com.lihaoyi/pprint
    api("com.lihaoyi:pprint_${vs.scala.artifactSuffix}:0.9.6")
}

dependencies {
    api(platform("org.apache.spark:spark-sql_${vs.scala.artifactSuffix}:${vs.spark.v}"))

    api("org.apache.logging.log4j:log4j-api")
    api("org.slf4j:slf4j-api")
}
