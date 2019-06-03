# Release process

## Shipping a new version

1. Ensure you are running on the oldest supported Erlang version

2. Update version in /VERSION

3. Ensure /CHANGELOG.md is updated, versioned and add the current date

4. Update "Compatibility and Deprecations" if a new OTP version is supported

5. Commit changes above with title "Release vVERSION" and generate a new tag

6. Run `make clean test` to ensure all tests pass from scratch and the CI is green

7. Recompile an existing project (for example, Ecto) to ensure manifests can be upgraded

8. Push branch and the new tag

9. Publish new zips with `make zips`, upload `Precompiled.zip` and `Docs.zip` to GitHub Releases, and include SHAs+CHANGELOG

10. Add the release to `elixir.csv` (all releases) and `_data/elixir-versions.yml` (except for RCs) files in `elixir-lang/elixir-lang.github.com`

11. Send an e-mail to elixir-lang-ann@googlegroups.com with title "Elixir vVERSION released". The body should be a link to the Release page on GitHub and the checksums. If it is a security release, prefix the title with the `[security]` tag

## Creating a new vMAJOR.MINOR branch

### In the new branch

1. Set `CANONICAL=` in /Makefile

2. Update tables in /SECURITY.md and "Compatibility and Deprecations"

3. Commit "Prepare vMAJOR.MINOR for release"

### Back in master

1. Bump /VERSION file

2. Start new /CHANGELOG.md

3. Update tables in /SECURITY.md in "Compatibility and Deprecations"

4. Commit "Start vMAJOR.MINOR+1"
