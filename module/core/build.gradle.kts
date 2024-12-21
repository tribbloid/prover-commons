val vs = versions()

dependencies {

    api(project(":prover-commons:infra"))
    testFixturesApi(testFixtures(project(":prover-commons:infra")))

    api("org.scalameta:ascii-graphs_${vs.scala.binaryV}:0.1.2")
//    api("org.typelevel:cats-effect_${vs.scala.binaryV}:3.4.5")

    api("com.chuusai:shapeless_${vs.scala.binaryV}:2.3.10")

    api("com.github.ben-manes.caffeine:caffeine:3.1.8")
}