import mill._
import mill.scalalib._
import mill.scalalib.publish._
import ammonite.ops._

trait PublishM2Module extends JavaModule with PublishModule {

  /**
    * Publish to the local Maven repository.
    * @param path The path to the local repository (default: `home / ".m2" / "repository"`).
    */
  def publishM2Local(path: Path = home / ".m2" / "repository") = T.command {
    T.ctx().log.info(s"Publish to ${path}")
    new LocalM2Publisher(path)
      .publish(
        jar = jar().path,
        sourcesJar = sourceJar().path,
        docJar = docJar().path,
        pom = pom().path,
        artifact = artifactMetadata()
      )
  }

}

class LocalM2Publisher(m2Repo: Path) {

  def publish(
               jar: Path,
               sourcesJar: Path,
               docJar: Path,
               pom: Path,
               artifact: Artifact
             ): Unit = {
    val releaseDir = m2Repo / artifact.group.split("[.]") / artifact.id / artifact.version
    writeFiles(
      jar -> releaseDir / s"${artifact.id}-${artifact.version}.jar",
      sourcesJar -> releaseDir / s"${artifact.id}-${artifact.version}-sources.jar",
      docJar -> releaseDir / s"${artifact.id}-${artifact.version}-javadoc.jar",
      pom -> releaseDir / s"${artifact.id}-${artifact.version}.pom"
    )
  }

  private def writeFiles(fromTo: (Path, Path)*): Unit = {
    fromTo.foreach {
      case (from, to) =>
        mkdir(to / up)
        cp.over(from, to)
    }
  }

}

