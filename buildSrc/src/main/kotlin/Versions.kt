import org.gradle.api.Project

class Versions(private val self: Project) {

    val projectVMajor = "1.0.0"
    val projectV = "$projectVMajor-SNAPSHOT"

    inner class Scala {
        val group: String = self.properties["scala.group"].toString()

        val v: String = self.properties["scala.version"].toString()
        protected val vParts: List<String> = v.split('.')

        val binaryV: String = vParts.subList(0, 2).joinToString(".")
        val minorV: String = vParts[2]
    }
    val scala = Scala()

    val scalaTestV = "3.2.12"
    val splainV: String = self.properties["splain.version"]?.toString() ?: ""
}