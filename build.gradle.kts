buildscript {
    repositories {
        // Add here whatever repositories you're already using
        mavenCentral()
    }

    dependencies {
        classpath("ai.acyclic:buildSrc")
        classpath("ch.epfl.scala:gradle-bloop_2.12:1.6.4") // suffix is always 2.12, weird
    }
}

plugins {
    id("ai.acyclic.scala2-conventions")
    id("ai.acyclic.scalatest-mixin")
//    id("ai.acyclic.publish-conventions")
}
