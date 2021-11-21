val vs = versions()

dependencies {
    testFixturesApi("com.chuusai:shapeless_${vs.scalaBinaryV}:2.3.3")

    testFixturesApi("org.scalatest:scalatest_${vs.scalaBinaryV}:${vs.scalaTestV}")
}