## Release process

This document simply outlines the release process:

1) Remove .dev extension from current versions

2) Run `make clean test` to ensure all tests pass from scratch

3) Ensure CHANGELOG is updated and tag release version with timestamp in it

4) Commit changes above and tag new version on Git

5) Release new docs, update elixir-lang.org

6) Push new zip to Elixir's downloads page

7) After release, bump versions and add .dev back

## Places where version is mentioned

* src/elixir.app.src
* lib/elixir/lib/system.ex
* rel/reltool.config
* Makefile
* CHANGELOG