## Release process

This document simply outlines the release process:

1) Remove `-dev` extension from VERSION

2) Run `make clean test` to ensure all tests pass from scratch and the CI is green

3) Ensure CHANGELOG is updated and tag release version with timestamp in it

4) Commit changes above with title "Release vVERSION"

5) Push master and create tag vVERSION from master branch

6) Update build info to include tag: `make build_info`

7) Release new docs with `make release_docs`, move docs to `docs/stable`

8) Release new zip with `make release_zip`, push new zip to GitHub Releases

9) Push package to expm with `expm publish package.exs`

10) Merge master into stable branch and push it

11) After release, bump versions, add `-dev` back and commit

12) `make release_docs` once again and push it to `elixir-lang.github.com`

## Places where version is mentioned

* VERSION
* CHANGELOG
* src/elixir.app.src
