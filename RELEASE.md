# Release process

This document simply outlines the release process:

1. Remove all `-dev` extension from versions (see below for all files)

2. Ensure CHANGELOG is updated and add current date

3. Commit changes above with title "Release vVERSION" and generate new tag

4. Run `make clean test` to ensure all tests pass from scratch and the CI is green

5. Push master and the new tag

6. Release new docs with `make release_docs`, move docs to `docs/stable`

7. Release new zip with `make release_zip`, push new zip to GitHub Releases, name it `Precompiled.zip`

8. Fast-forward merge master into stable branch with `git merge master --ff` and push it

9. After release, bump versions, add `-dev` back and commit

10. `make release_docs` once again and push it to `elixir-lang/docs`

11. Add the release to `elixir.csv` file in `elixir-lang/elixir-lang.github.com`

## Places where version is mentioned

* VERSION (make sure there is no newline in this file)
* CHANGELOG.md
* src/elixir.app.src (not lib/elixir/src/elixir.app.src)
