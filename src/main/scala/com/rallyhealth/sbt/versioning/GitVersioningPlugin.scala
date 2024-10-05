package bleep
package plugin.versioning

import bleep.plugin.versioning.GitFetcher.FetchResult
import bleep.plugin.versioning.LowerBoundedSemanticVersion.*

import java.nio.file.Path
import scala.concurrent.duration.*

/** Enforces Semantic Version plus support for identifying "x.y.z-SNAPSHOT" and "x.y.z-dirty-SNAPSHOT" builds.
  */
class GitVersioningPlugin(baseDirectory: Path, logger: ryddig.Logger)(
    // `autoFetch` indicates whether to auto-fetch tags from remotes
    val autoFetch: Boolean = Option(System.getProperty("version.autoFetch"))
      .orElse(Option(System.getenv("VERSION_AUTOFETCH")))
      .exists(_.toBoolean),
    // `autoFetchTags` are the names of git remotes to fetch tags from with `autoFetch` is true
    val autoFetchRemotes: Seq[String] = Seq("upstream", "origin"),
    // `autoFetchTimeout` is the timeout for git auto-fetching (in seconds)
    val autoFetchTimeout: Int = 15, // seconds
    /** The sbt ecosystem relies on the value of the version setting key as the source of truth for the version we're currently building.
      *
      * GitVersioningPlugin is based on git tags which are often incremented to later versions after CI builds complete.
      *
      * For example, given a most recent tag of v1.5.0, [[GitVersioningPlugin]] at CI time will determine the version to be something like
      * v1.5.1-4-aabcdef-SNAPSHOT.
      *
      * If we have checks (MiMa, SemVerPlugin, ShadingPlugin) that enforce that no major changes are introduced, without correctly versioning as
      * v2.0.0-whatever-SNAPSHOT, then the build would fail, incorrectly.
      *
      * [[gitVersioningSnapshotLowerBound]] offers a way to nudge the candidate version up to v2.0.0-4-aabcdef-SNAPSHOT without tagging:
      *
      * {{{
      *   gitVersioningSnapshotLowerBound := "2.0.0"
      * }}}
      *
      * [[gitVersioningSnapshotLowerBound]] acts as a lower bound and has no effect when its value is less than [[versionFromGit]].
      *
      * [[gitVersioningSnapshotLowerBound]] has no effect when [[versionOverride]] is present.
      *
      * Produces snapshot versions whose major.minor.patch is at least this version.
      */
    val gitVersioningSnapshotLowerBound: Option[LowerBound] = None,
    /** This is used by our Jenkins scripts to set the version before creating and publishing a release. This must be a valid [[SemanticVersion]].
      *
      * The old `versionClassifier` was merged into this because it made the domain model excessively complicated -- it was part of the version but carried
      * around separately until some arbitrary point where it was merged into the version.
      *
      * `versionOverride` overrides the automatically determined `version`. This is set (often by a system property) to the version you want to release before
      * executing `publish` or `publishLocal`
      */
    val versionOverride: Option[String] = Option(System.getProperty("version.override")).map { versionOverrideStr =>
      val ver = ReleaseVersion
        .unapply(versionOverrideStr)
        .filter(!_.isDirty)
        .getOrElse {
          val msg = s"cannot parse versionOverride=$versionOverrideStr as clean release version"
          throw new IllegalArgumentException(msg)
        }

      logger.info(s"GitVersioningPlugin set versionOverride=$versionOverrideStr")
      ver.toString
    },
    // Forces clean builds, i.e. doesn't add '-dirty' to the version.
    val ignoreDirty: Boolean = false,
    // Specifies that the version from git should be incremented as a major, minor, or patch release.
    val gitVersioningMaybeRelease: Option[SemVerReleaseType] = sys.props.get("release").map(SemVerReleaseType.fromStringOrThrow)
) {

  // Driver that allows executing common commands against a git working directory.
  lazy val gitDriver: GitDriver = new GitDriverImpl(baseDirectory.toFile)

  // `autoFetchResult` is the result of the auto-fetch
  def autoFetchResult() =
    if (autoFetch) {
      logger.info("Fetching the most up-to-date tags from git remotes")
      GitFetcher.fetchRemotes(autoFetchTimeout.seconds)(logger)
    } else {
      logger.info("Skipping fetching tags from git remotes; to enable, set the system property version.autoFetch=true")
      Seq.empty[FetchResult]
    }

  /** See [[GitDriver.calcCurrentVersion()]] `versionFromGit` is The version as determined by git history
    */
  def versionFromGit: SemanticVersion = {
    // This depends on but does not use [[autoFetchResult]]; that ensures the task is run but ignores the result.
    autoFetchResult().discard()
    val gitVersion = gitDriver.calcCurrentVersion(ignoreDirty)

    logger.info(s"GitVersioningPlugin set versionFromGit=$gitVersion")

    gitVersion
  }

  def version = semanticVersion.toString

  // The typed representation of `version`
  def semanticVersion: SemanticVersion = {
    import SemVerReleaseType.*
    // version must be semver, see version's definition
    val verOverride = versionOverride.map(
      SemanticVersion
        .fromString(_)
        .getOrElse(throw new IllegalArgumentException(s"cannot parse version=${versionOverride}"))
    )

    gitVersioningMaybeRelease.foreach { release =>
      logger.info(s"GitVersioningPlugin set release=$release")
    }

    gitVersioningSnapshotLowerBound.foreach { bound =>
      logger.info(s"GitVersioningPlugin set gitVersioningSnapshotLowerBound=$bound")
    }

    lazy val boundedVersionFromGit: SemanticVersion = {
      // 1. Start with version from git.
      // 2. Apply major/minor/patch release.
      // 3. Apply snapshot lower bound.
      type VersionTransform = SemanticVersion => SemanticVersion

      val applyMajorMinorPatchRelease: VersionTransform = ver =>
        gitVersioningMaybeRelease
          .foldLeft(ver)(_.release(_))

      val applySnapshotLowerBound: VersionTransform = ver =>
        gitVersioningSnapshotLowerBound
          .foldLeft(ver)(_.lowerBound(_, gitDriver.branchState))

      val applyAllTransforms = applyMajorMinorPatchRelease andThen applySnapshotLowerBound
      applyAllTransforms(versionFromGit)
    }

    val version = verOverride.getOrElse(boundedVersionFromGit)
    logger.info(s"GitVersioningPlugin set version=$version")
    version
  }

  /** `isCleanRelease` is whether the 'version' is a clean release or not (NOT based on the working dir) Note this can be overriden by [[ignoreDirty]].
    */
  def isCleanRelease(): Boolean = {
    // version must be semver, see version's definition
    val isDirty = semanticVersion.isDirty

    logger.info(s"GitVersioningPlugin set isCleanRelease=${!isDirty}")

    !isDirty
  }

  // Prints the version that would be applied to this sbt project
  def printVersion(): Unit = {
    logger.info(s"Version as determined by git history: ${versionFromGit}")
    versionOverride.foreach { verOverride =>
      logger.info(s"Version as determined by system property (version.override): $verOverride")
    }
    gitVersioningMaybeRelease.foreach { relType =>
      logger.info(s"Release type: $relType")
    }
    logger.info(s"Successfully determined version: ${versionOverride.getOrElse(versionFromGit)}")
  }
}
