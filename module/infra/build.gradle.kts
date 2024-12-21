val vs = versions()

dependencies {
//scala compiler
    api("org.scala-lang:scala-compiler:${vs.scala.v}")

//    api("org.apache.logging.log4j:log4j:2.11.2")
    api("org.apache.logging.log4j:log4j-slf4j-impl:2.20.0")

    // https://mvnrepository.com/artifact/com.lihaoyi/sourcecode
    api("com.lihaoyi:sourcecode_2.13:0.4.2")

    // https://mvnrepository.com/artifact/com.lihaoyi/pprint
    api("com.lihaoyi:pprint_2.13:0.9.0")
}