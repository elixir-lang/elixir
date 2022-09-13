# Release process

## Shipping a new version

1. Ensure you are running on the oldest supported Erlang version

2. Update version in /VERSION, bin/elixir and bin/elixir.bat

3. Ensure /CHANGELOG.md is updated, versioned and add the current date

4. Update "Compatibility and Deprecations" if a new OTP version is supported

5. Commit changes above with title "Release vVERSION", generate a new tag, and push it

6. Wait until GitHub Actions publish artifacts to the draft release and the CI is green

7. Copy the relevant bits from /CHANGELOG.md to the GitHub release and publish it

8. Add the release to `elixir.csv` with the minimum supported OTP version (all releases), update `erlang.csv` to the latest supported OTP version, and `_data/elixir-versions.yml` (except for RCs) files in `elixir-lang/elixir-lang.github.com`

## Creating a new vMAJOR.MINOR branch

### In the new branch

1. Set `CANONICAL=` in /Makefile

2. Update tables in /SECURITY.md and "Compatibility and Deprecations"

3. Commit "Branch out vMAJOR.MINOR"

### Back in main

1. Bump /VERSION file, bin/elixir and bin/elixir.bat

2. Start new /CHANGELOG.md

3. Update tables in /SECURITY.md and in "Compatibility and Deprecations"

4. Commit "Start vMAJOR.MINOR+1"

## Changing supported Erlang/OTP versions

1. Update the table in Compatibility and Deprecations

2. Update `otp_release` checks in `/Makefile` and `/lib/elixir/src/elixir.erl`

3. Update relevant CI workflows in `/.github/workflows/*.yml`

4. Remove `otp_release` version checks that are no longer needed
