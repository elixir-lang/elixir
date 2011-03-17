## Release process

This document simply outlines the release process:

1) Run `make clean test` to ensure all tests pass from scratch

2) Remove .dev extension from current versions

3) Ensure CHANGELOG is updated and tag release version with timestamp in it

4) Commit changes above and tag new version on Git

5) After release, bump versions and add .dev back

## Places where version is mentioned

* ebin/elixir.app
* lib/code.ex
* README
* CHANGELOG