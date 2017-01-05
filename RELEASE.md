# Release process

## All releases

This document simply outlines the release process:

1. Ensure you are running on the oldest supported Erlang version

2. Remove all `-dev` extension from versions (see below for all files)

3. Ensure CHANGELOG is updated and add current date

4. Commit changes above with title "Release vVERSION" and generate new tag

5. Run `make clean test` to ensure all tests pass from scratch and the CI is green

6. Recompile an existing project (for example, Ecto) to ensure manifests can be upgraded

7. Push branch and the new tag

8. If a new `vMAJOR.MINOR`, create a new branch "vMAJOR.MINOR" and set `CANONICAL=` in Makefile before building docs

9. Publish new zips with `make zips`, upload `Precompiled.zip` and `Docs.zip` to GitHub Releases

10. Add the release to `elixir.csv` file in `elixir-lang/elixir-lang.github.com`

11. Update `docs.markdown` in `elixir-lang/elixir-lang.github.com`. If a new `vMAJOR.MINOR`, create a section containing the links to each library documentation. If a `vMAJOR.MINOR.PATCH`, update the doc links in the corresponding section

12. After a new `vMAJOR.MINOR`, move back to master, bump versions, start new CHANGELOG, add `-dev` back and commit "Start vMAJOR.MINOR+1"

## Places where version is mentioned

* VERSION
* CHANGELOG.md
* src/elixir.app.src (not lib/elixir/src/elixir.app.src)

## Deprecation policy

Elixir deprecations happens in 3 steps:

  1. The feature is soft-deprecated. It means both CHANGELOG and documentation must list the feature as deprecated but no warning is effectively emitted by running the code. There is no requirement to soft-deprecate a feature.

  2. The feature is effectively deprecated by emitting warnings on usage. In order to deprecate a feature, the proposed alternative MUST exist for AT LEAST two versions. For example, `Enum.uniq/2` was soft-deprecated in favor of `Enum.uniq_by/2` in Elixir v1.1. This means a deprecation warning may only be emitted by Elixir v1.3 or later.

  3. The feature is removed. This can only happen on major releases. This means deprecated features in Elixir v1.x shall only be removed by Elixir v2.x.
