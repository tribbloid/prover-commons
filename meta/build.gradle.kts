val vs = versions()

val splainVRelease: String = "1.0.1"

val splainVFallback: String = run {
    vs.splainV.ifEmpty { splainVRelease }
}

dependencies {

    implementation("${vs.scala.group}:scala-compiler:${vs.scala.v}")
    implementation("${vs.scala.group}:scala-reflect:${vs.scala.v}")

    api(project(":core"))
    testFixturesApi(testFixtures(project(":core")))
}