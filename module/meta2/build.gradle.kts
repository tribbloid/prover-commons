val vs = versions()

dependencies {

    implementation("${vs.scala.group}:scala-compiler:${vs.scala.v}")
    implementation("${vs.scala.group}:scala-reflect:${vs.scala.v}")

    api(project(":prover-commons:core"))
    testFixturesApi(testFixtures(project(":prover-commons:core")))
}