package ai.acyclic.prover.commons

object Envs {
  import ai.acyclic.prover.commons.util.PathMagnet.*
  // TODO: most instances here should be compile-time

  val USER_HOME: LocalFSPath = LocalFSPath(System.getProperty("user.home"))
  val USER_DIR: LocalFSPath = LocalFSPath(System.getProperty("user.dir"))

  val TEMP: String = "temp"

  val USER_TEMP_DIR: LocalFSPath = USER_DIR :\ TEMP
  val ROOT_TEMP_DIR: LocalFSPath = LocalFSPath(System.getProperty("java.io.tmpdir"))
}
