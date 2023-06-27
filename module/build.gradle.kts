val vs = versions()

allprojects {
    group = "ai.acyclic.prover-commons"

    dependencies {

        // see https://github.com/gradle/gradle/issues/13067
        fun both(notation: Any) {
            implementation(notation)
            testFixturesImplementation(notation)
        }

        implementation("${vs.scala.group}:scala-library:${vs.scala.v}")

        testFixturesApi("org.scalatest:scalatest_${vs.scala.binaryV}:${vs.scalaTestV}")

        testImplementation("org.junit.jupiter:junit-jupiter:5.9.2")
        testRuntimeOnly("co.helmethair:scalatest-junit-runner:0.2.0")
    }
}