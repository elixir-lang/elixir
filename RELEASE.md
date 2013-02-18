## Release process

This document simply outlines the release process:

1) Remove `.dev` extension from VERSION

2) Run `make clean test` to ensure all tests pass from scratch and the CI is green

3) Ensure CHANGELOG is updated and tag release version with timestamp in it

4) Commit changes above and update stable branch

5) Create tag from master branch

6) Release new docs with `make release_docs`, update elixir-lang.org

7) Release new zip with `make release_zip`, push new zip to Elixir's elixir-lang.org/packages.html

8) Push package to expm with `expm publish package.exs`

9) After release, bump versions and add `.dev` back

## Places where version is mentioned

* VERSION
* CHANGELOG
* src/elixir.app.src
