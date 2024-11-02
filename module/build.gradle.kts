val vs = versions()

allprojects {
    group = "ai.acyclic.prover-commons"

    dependencies {

        scalaCompilerPlugins("org.typelevel:kind-projector_${vs.scala.v}:0.13.3")
//        TODO: enable for all project as it is an important syntax

//        TODO: use "-P:kind-projector:underscore-placeholders" once migrated to source:3

    }

    tasks {

        withType<ScalaCompile> {

            scalaCompileOptions.apply {

                additionalParameters.addAll(
                    listOf(
//                        "-Xsource:3-cross",
                        "-Xsource:3",
                        // quickfix should be disabled ASAP after migration
//                        "-quickfix",
//                        "-quickfix:cat=scala3-migration"

                    )
                )
            }
        }
    }
}
