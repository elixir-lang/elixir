# Release process

This document simply outlines the release process:

1. Remove `-dev` extension from VERSION

2. Ensure CHANGELOG is updated and timestamp

3. Commit changes above with title "Release vVERSION" and generate new tag

4. Run `make clean test` to ensure all tests pass from scratch and the CI is green

5. Push master and tags

6. Release new docs with `make release_docs`, move docs to `docs/stable`

7. Release new zip with `make release_zip`, push new zip to GitHub Releases

8. Merge master into stable branch and push it

9. After release, bump versions, add `-dev` back and commit

10. `make release_docs` once again and push it to `elixir-lang.org`

11. Also update `release` file in `elixir-lang.org`

## Places where version is mentioned

* VERSION
* CHANGELOG
* src/elixir.app.src
