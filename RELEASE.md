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

10. Add the release to `elixir.csv` and `_data/elixir-versions.yml` files in `elixir-lang/elixir-lang.github.com`

11. After a new `vMAJOR.MINOR`, move back to master, bump versions, start new CHANGELOG, add `-dev` back and commit "Start vMAJOR.MINOR+1"

## Places where version is mentioned

* VERSION
* CHANGELOG.md
* src/elixir.app.src (not lib/elixir/src/elixir.app.src)
