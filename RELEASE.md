# Release process

## All releases

This document simply outlines the release process:

1. Ensure you are running on the oldest supported Erlang version

2. Remove all `-dev` extension from versions (see below for all files)

3. Ensure CHANGELOG is updated and add current date

4. If a new `vMAJOR.MINOR`, update in [Deprecations](lib/elixir/pages/Deprecations.md) page the link
   to [vVERSION]'s changelog (located at the end of the file) by replacing "master" with "vVERSION"

5. Commit changes above with title "Release vVERSION" and generate new tag

6. Run `make clean test` to ensure all tests pass from scratch and the CI is green

7. Recompile an existing project (for example, Ecto) to ensure manifests can be upgraded

8. Push branch and the new tag

9. If a new `vMAJOR.MINOR`, create a new branch "vMAJOR.MINOR" and set `CANONICAL=` in Makefile before building docs

10. Publish new zips with `make zips`, upload `Precompiled.zip` and `Docs.zip` to GitHub Releases

11. Add the release to `elixir.csv` and `_data/elixir-versions.yml` files in `elixir-lang/elixir-lang.github.com`

12. After a new `vMAJOR.MINOR`, move back to master, bump versions, start new CHANGELOG, add `-dev` back and commit "Start vMAJOR.MINOR+1"

## Places where version is mentioned

* VERSION
* CHANGELOG.md
* src/elixir.app.src (not lib/elixir/src/elixir.app.src)
