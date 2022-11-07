package bleep.plugin.semver.level.rule

import bleep.plugin.semver.level.{SemVerEnforcementLevel, SemVerLevelRule}
import bleep.plugin.versioning.SemVerReleaseType.Major
import bleep.plugin.versioning.SemanticVersion

/** @see
  *   http://semver.org/#spec-item-4
  */
case class InitialDevelopmentRule(current: SemanticVersion) extends SemVerLevelRule {

  override def calcLevel(): Option[SemVerEnforcementLevel] =
    if (current.isInitialDevVersion) {
      Some(MajorChangesAllowedForInitialDevelopment)
    } else {
      None
    }
}

/** @see
  *   http://semver.org/#spec-item-4
  */
case object MajorChangesAllowedForInitialDevelopment
    extends SemVerEnforcementLevel(
      releaseType = Major,
      explanation = "Major version zero (0.y.z) is for initial development. Anything may change at any time. The public API should not be considered stable."
    )
