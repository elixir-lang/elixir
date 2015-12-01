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

8. Publish new docs with `make publish_docs`, copy docs to `docs/stable` if appropriate, and push to GitHub Pages

9. Publish new zips with `make publish_zips`, upload `Precompiled.zip` and `Docs.zip` to GitHub Releases

10. Add the release to `elixir.csv` file in `elixir-lang/elixir-lang.github.com`

11. Build and push standalone Mix with `make publish_mix` (requires AWS credentials)

## New vMAJOR.MINOR releases

12. Create a new branch "vMAJOR.MINOR"

13. Move docs generation to `docs/vMAJOR.MINOR` in Makefile and copy them from `docs/stable`

14. In master, bump versions, start new CHANGELOG, add `-dev` back and commit "Start vVERSION+1"

15. `make release_docs` and push it to `elixir-lang/docs`

## Places where version is mentioned

* VERSION (make sure there is no newline in this file)
* CHANGELOG.md
* src/elixir.app.src (not lib/elixir/src/elixir.app.src)
