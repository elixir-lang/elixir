# Contributing to Elixir

We welcome everyone to contribute to Elixir. To do so, there are a few things you need to know about the code. First, Elixir code is divided in applications inside the `lib` folder:

* `elixir` - Elixir's kernel and standard library

* `eex` - EEx is the template engine that allows you to embed Elixir

* `ex_unit` - ExUnit is a simple test framework that ships with Elixir

* `iex` - IEx stands for Interactive Elixir: Elixir's interactive shell

* `logger` - Logger is the built-in logger

* `mix` - Mix is Elixir's build tool

You can run all tests in the root directory with `make test` and you can also run tests for a specific framework `make test_#{APPLICATION}`, for example, `make test_ex_unit`. If you just changed something in the Elixir's standard library, you can run only that portion through `make test_stdlib`.

If you are changing just one file, you can choose to compile and run tests only for that particular file for fast development cycles. For example, if you are changing the String module, you can compile it and run its tests as:

```sh
bin/elixirc lib/elixir/lib/string.ex -o lib/elixir/ebin
bin/elixir lib/elixir/test/elixir/string_test.exs
```

To recompile (including Erlang modules):

```sh
make compile
```

After your changes are done, please remember to run `mix format` to guarantee all files are properly formatted and then run the full suite with `make test`.

If your contribution fails during the bootstrapping of the language, you can rebuild the language from scratch with:

```sh
make clean_elixir compile
```

Similarly, if you can't get Elixir to compile or the tests to pass after updating an existing checkout, run `make clean compile`. You can check [the official build status on Travis-CI](https://travis-ci.org/elixir-lang/elixir). More tasks can be found by reading the [Makefile](Makefile).

With tests running and passing, you are ready to contribute to Elixir and [send a pull request](https://help.github.com/articles/using-pull-requests/). We have saved some excellent pull requests we have received in the past in case you are looking for some examples:

* [Implement Enum.member? - Pull Request](https://github.com/elixir-lang/elixir/pull/992)
* [Add String.valid? - Pull Request](https://github.com/elixir-lang/elixir/pull/1058)
* [Implement capture_io for ExUnit - Pull Request](https://github.com/elixir-lang/elixir/pull/1059)

## Reviewing changes

Once a pull request is sent, the Elixir team will review your changes. We outline our process below to clarify the roles of everyone involved.

All pull requests must be approved by two committers before being merged into the repository. If any changes are necessary, the team will leave appropriate comments requesting changes to the code. Unfortunately we cannot guarantee a pull request will be merged, even when modifications are requested, as the Elixir team will re-evaluate the contribution as it changes.

Committers may also push style changes directly to your branch. If you would rather manage all changes yourself, you can disable "Allow edits from maintainers" feature when submitting your pull request.

The Elixir team may optionally assign someone to review a pull request. If someone is assigned, they must explicitly approve the code before another team member can merge it.

When the review finishes, your pull request will be squashed and merged into the repository. If you have carefully organized your commits and believe they should be merged without squashing, please mention it in a comment.
