val vs = versions()

allprojects {
    group = "ai.acyclic.prover-commons"

    dependencies {

        scalaCompilerPlugins("org.typelevel:kind-projector_${vs.scala.v}:0.13.2")
//        TODO: enable for all project as it is an important syntax

//        TODO: use "-P:kind-projector:underscore-placeholders" once migrated to source:3
    }
}