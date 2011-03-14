## Release process

This document simply outlines the release process:

1) Run `make clean test` to ensure all tests pass from scratch

2) Ensure CHANGELOG is updated and tag latest version with timestamp in it

3) Commit changes above Release elixir by tagging it on Git

4) After release, update versions on `ebin/elixir.app` and `lib/code.ex`

5) After release, add a "Current master" header to the CHANGELOG