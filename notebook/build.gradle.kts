val vs = versions()

dependencies {
    api(project(":prover-commons:core"))

    testFixturesApi(testFixtures(project(":prover-commons:core")))

    testImplementation("org.junit.jupiter:junit-jupiter:5.9.2")
    testRuntimeOnly("co.helmethair:scalatest-junit-runner:0.2.0")
}