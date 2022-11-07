package bleep.plugin.semver.level.rule

import bleep.plugin.semver.level.{SemVerEnforcementLevel, SemVerLevelRule}
import bleep.plugin.versioning.SemVerReleaseType.Major
import bleep.plugin.versioning.{ReleaseVersion, SemanticVersion}

case class EnforceAfterVersionRule(current: SemanticVersion, maybeEnforceAfterVersion: Option[ReleaseVersion]) extends SemVerLevelRule {

  override def calcLevel() = {
    maybeEnforceAfterVersion
      .filter(current <= _)
      .map(enforceAfterVersion => DisabledEnforceAfterVersion(enforceAfterVersion))
  }
}

case class DisabledEnforceAfterVersion(enforceAfterVersion: ReleaseVersion) extends SemVerEnforcementLevel(
  releaseType = Major,
  explanation = s"semVerEnforceAfterVersion := $enforceAfterVersion was used to allow major changes until the specified version."
)
