# Release process

## All releases

This document simply outlines the release process:

1. Ensure you are running on the oldest supported Erlang version

1. Remove all `-dev` extension from versions (see below for all files)

1. Ensure CHANGELOG is updated and add current date

1. If a new `vMAJOR.MINOR`, replace "master" with "vVERSION" in the "Compatibility and Deprecations" page and commit

1. If a new `vMAJOR.MINOR`, create a new branch "vMAJOR.MINOR" and set `CANONICAL=` in Makefile

1. Add an entry for the new version to the OTP compatibility table in the "Compatibility and Deprecations" page

1. Commit changes above with title "Release vVERSION" and generate new tag

1. Run `make clean test` to ensure all tests pass from scratch and the CI is green

1. Recompile an existing project (for example, Ecto) to ensure manifests can be upgraded

1. Push branch and the new tag

1. Publish new zips with `make zips`, upload `Precompiled.zip` and `Docs.zip` to GitHub Releases

1. Add the release to `elixir.csv` and `_data/elixir-versions.yml` files in `elixir-lang/elixir-lang.github.com`

1. After a new `vMAJOR.MINOR`, move back to master, bump versions, start new CHANGELOG, add `-dev` back and commit "Start vMAJOR.MINOR+1"

## Places where version is mentioned

* VERSION
* CHANGELOG.md
* lib/elixir/src/elixir.app.src
