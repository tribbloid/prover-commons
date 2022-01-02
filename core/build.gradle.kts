val vs = versions()

dependencies {
    testFixturesApi("com.chuusai:shapeless_${vs.scalaBinaryV}:2.3.7")

    testFixturesApi("org.scalatest:scalatest_${vs.scalaBinaryV}:${vs.scalaTestV}")
}