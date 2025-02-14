/*
 * Copyright (C) 2019 The ORT Project Authors (see <https://github.com/oss-review-toolkit/ort/blob/main/NOTICE>)
 * Copyright (c) 2021 The Elixir Team
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

 // Docs: https://oss-review-toolkit.org/ort/docs/configuration/evaluator-rules

val whitelistedLicenses = listOf(
  // License for Elixir & Imported Erlang Projects
  "Apache-2.0",
  // License for the Elixir Logo
  "LicenseRef-elixir-trademark-policy",
  // License for included Unicode Files
  "LicenseRef-scancode-unicode",
  // DCO for committers
  "LicenseRef-scancode-dco-1.1"
).map { SpdxSingleLicenseExpression.parse(it) }.toSet()

fun PackageRule.howToFixDefault() = """
      * Check if this license violation is intended
      * Adjust evaluation rules in `.ort/config/evaluator.rules.kts`
    """.trimIndent()

fun PackageRule.LicenseRule.isHandled() =
  object : RuleMatcher {
    override val description = "isHandled($license)"

    override fun matches() = license in whitelistedLicenses
  }

fun RuleSet.unhandledLicenseRule() = packageRule("UNHANDLED_LICENSE") {
  // Do not trigger this rule on packages that have been excluded in the .ort.yml.
  require {
    -isExcluded()
  }

  // Define a rule that is executed for each license of the package.
  licenseRule("UNHANDLED_LICENSE", LicenseView.CONCLUDED_OR_DECLARED_AND_DETECTED) {
    require {
      -isExcluded()
      -isHandled()
    }

    // Throw an error message including guidance how to fix the issue.
    error(
      "The license $license is currently not covered by policy rules. " +
        "The license was ${licenseSource.name.lowercase()} in package " +
        "${pkg.metadata.id.toCoordinates()}.",
      howToFixDefault()
    )
  }
}

fun RuleSet.unmappedDeclaredLicenseRule() = packageRule("UNMAPPED_DECLARED_LICENSE") {
  require {
    -isExcluded()
  }

  resolvedLicenseInfo.licenseInfo.declaredLicenseInfo.processed.unmapped.forEach { unmappedLicense ->
    warning(
      "The declared license '$unmappedLicense' could not be mapped to a valid license or parsed as an SPDX " +
        "expression. The license was found in package ${pkg.metadata.id.toCoordinates()}.",
      howToFixDefault()
    )
  }
}

val ruleSet = ruleSet(ortResult, licenseInfoResolver, resolutionProvider) {
  unhandledLicenseRule()
  unmappedDeclaredLicenseRule()
}

ruleViolations += ruleSet.violations
