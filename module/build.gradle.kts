val vs = versions()

allprojects {
    group = "ai.acyclic.prover-commons"

    dependencies {

        testFixturesApi("org.scalatest:scalatest_${vs.scala.binaryV}:${vs.scalaTestV}")

        testImplementation("org.junit.jupiter:junit-jupiter:5.9.2")
        testRuntimeOnly("co.helmethair:scalatest-junit-runner:0.2.0")
    }
}