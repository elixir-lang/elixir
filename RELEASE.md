<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Release process

## Shipping a new version

1. Update version in /VERSION, bin/elixir, and bin/elixir.bat

2. Ensure /CHANGELOG.md is updated, versioned and add the current date

3. Update "Compatibility and Deprecations" if a new OTP version is supported

4. Commit changes above with title "Release vVERSION" and push it

5. Once GitHub actions completes, generate a new tag, and push it

6. Wait until GitHub Actions publish artifacts to the draft release

7. Copy the relevant bits from /CHANGELOG.md to the GitHub release and publish it (link to the announcement if there is one)

8. Update `_data/elixir-versions.yml` (except for RCs) in `elixir-lang/elixir-lang.github.com`

## Creating a new vMAJOR.MINOR branch (usually before first rc)

### In the new branch

1. Comment out `CANONICAL := main/` in /Makefile

2. Update tables in /SECURITY.md and "Compatibility and Deprecations"

3. Commit "Branch out vMAJOR.MINOR"

### Back in main

1. Bump /VERSION file, bin/elixir, and bin/elixir.bat

2. Start new /CHANGELOG.md

3. Update tables in /SECURITY.md and in "Compatibility and Deprecations"

4. Commit "Start vMAJOR.MINOR+1"

## Changing supported Erlang/OTP versions

1. Update the table in Compatibility and Deprecations

2. Update `otp_release` checks in `/Makefile` and `/lib/elixir/src/elixir.erl`

3. Update relevant CI workflows in `/.github/workflows/*.yml` - for release workflows, outdated/recently added Erlang/OTP versions must run conditionally

4. Remove `otp_release` version checks that are no longer needed
