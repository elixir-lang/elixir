![Elixir](https://github.com/elixir-lang/elixir-lang.github.com/raw/master/images/logo/logo.png)
=========
[![Travis build](https://secure.travis-ci.org/elixir-lang/elixir.svg?branch=master
"Build Status")](https://travis-ci.org/elixir-lang/elixir)

Elixir is a dynamic, functional language designed for building scalable
and maintainable applications.

For more about Elixir, installation and documentation,
[check Elixir's website](https://elixir-lang.org/).

## Announcements

New releases are announced in the [announcements mailing list](https://groups.google.com/group/elixir-lang-ann). All security releases [will be tagged with `[security]`](https://groups.google.com/forum/#!searchin/elixir-lang-ann/%5Bsecurity%5D%7Csort:date).

## Compiling from source

To run Elixir from source, clone this repository to your machine, compile and test it:

```sh
git clone https://github.com/elixir-lang/elixir.git
cd elixir
make clean test
```

> Note: if you are running on Windows,
[this article includes important notes for compiling Elixir from source
on Windows](https://github.com/elixir-lang/elixir/wiki/Windows).

If Elixir fails to build (specifically when pulling in a new version via
`git`), be sure to remove any previous build artifacts by running
`make clean`, then `make test`.

If tests pass, you are ready to move on to the [Getting Started guide][1]
or to try Interactive Elixir by running `bin/iex` in your terminal.

However, if tests fail, it is likely you have an outdated Erlang/OTP version
(Elixir requires Erlang/OTP 20.0 or later). You can check your Erlang/OTP version
by calling `erl` in the command line. You will see some information as follows:

    Erlang/OTP 20 [erts-9.0] [smp:2:2] [async-threads:10] [kernel-poll:false]

If you have properly set up your dependencies and tests still fail,
you may want to open up a bug report, as explained next.

## Bug reports

For reporting bugs, [visit our issues tracker][2] and follow the steps
for reporting a new issue. **Please disclose security vulnerabilities
privately at elixir-security@googlegroups.com**.

## Proposing new features

For proposing new features, please start a discussion in the
[Elixir Core mailing list][3]. Keep in mind that it is your responsibility
to argue and explain why a feature is useful and how it will impact the
codebase and the community.

Once a proposal is accepted, it will be added to [the issues tracker][2].
The issues tracker focuses on *actionable items* and it holds a list of
upcoming enhancements and pending bugs. All entries in the tracker are
tagged for clarity and to ease collaboration.

Features and bug fixes that have already been merged and will be included
in the next release are marked as "closed" in the issues tracker and are
added to the [CHANGELOG](CHANGELOG.md).

Finally, remember all interactions in our official spaces follow our
[Code of Conduct][7].

## Contributing

We welcome everyone to contribute to Elixir. To do so, there are a few
things you need to know about the code. First, Elixir code is divided
in applications inside the `lib` folder:

* `elixir` - Contains Elixir's kernel and stdlib

* `eex` - Template engine that allows you to embed Elixir

* `ex_unit` - Simple test framework that ships with Elixir

* `iex` - IEx, Elixir's interactive shell

* `logger` - The built-in logger

* `mix` - Elixir's build tool

You can run all tests in the root directory with `make test` and you can
also run tests for a specific framework `make test_#{NAME}`, for example,
`make test_ex_unit`. If you just changed something in the Elixir's standard
library, you can run only that portion through `make test_stdlib`.

If you are changing just one file, you can choose to compile and run tests only
for that particular file for fast development cycles. For example, if you
are changing the String module, you can compile it and run its tests as:

```sh
bin/elixirc lib/elixir/lib/string.ex -o lib/elixir/ebin
bin/elixir lib/elixir/test/elixir/string_test.exs
```

To recompile (including Erlang modules):

```sh
make compile
```

After your changes are done, please remember to run `mix format` to guarantee
all files are properly formatted and then run the full suite with
`make test`.

If your contribution fails during the bootstrapping of the language,
you can rebuild the language from scratch with:

```sh
make clean_elixir compile
```

Similarly, if you can't get Elixir to compile or the tests to pass after
updating an existing checkout, run `make clean compile`. You can check
[the official build status on Travis-CI](https://travis-ci.org/elixir-lang/elixir).
More tasks can be found by reading the [Makefile](./Makefile).

With tests running and passing, you are ready to contribute to Elixir and
[send a pull request](https://help.github.com/articles/using-pull-requests/).
We have saved some excellent pull requests we have received in the past in
case you are looking for some examples:

* [Implement Enum.member? – Pull Request](https://github.com/elixir-lang/elixir/pull/992)
* [Add String.valid? – Pull Request](https://github.com/elixir-lang/elixir/pull/1058)
* [Implement capture_io for ExUnit – Pull Request](https://github.com/elixir-lang/elixir/pull/1059)

### Reviewing changes

Once a pull request is sent, the Elixir team will review your changes.
We outline our process below to clarify the roles of everyone involved.

All pull requests must be approved by two committers before being merged into
the repository. If any changes are necessary, the team will leave appropriate
comments requesting changes to the code. Unfortunately we cannot guarantee a
pull request will be merged, even when modifications are requested, as the Elixir
team will re-evaluate the contribution as it changes.

Committers may also push style changes directly to your branch. If you would
rather manage all changes yourself, you can disable "Allow edits from maintainers"
feature when submitting your pull request.

The Elixir team may optionally assign someone to review a pull request.
If someone is assigned, they must explicitly approve the code before
another team member can merge it.

When the review finishes, your pull request will be squashed and merged
into the repository. If you have carefully organized your commits and
believe they should be merged without squashing, leave a comment.

## Building documentation

Building the documentation requires [ExDoc](https://github.com/elixir-lang/ex_doc)
to be installed and built alongside Elixir:

```sh
# After cloning and compiling Elixir, in its parent directory:
git clone git://github.com/elixir-lang/ex_doc.git
cd ex_doc && ../elixir/bin/mix do deps.get, compile
cd ../elixir && make docs
```

This will produce documentation sets for `elixir`, `mix`, etc. under
the `doc` directory. If you are planning to contribute documentation,
[please check our best practices for writing documentation](https://hexdocs.pm/elixir/writing-documentation.html).

## Development links

  * [Elixir Getting Started guide][1]
  * [Elixir Documentation][6]
  * [Elixir Core Mailing list (development)][3]
  * [Issues tracker][2]
  * [Code of Conduct][7]
  * **[#elixir-lang][4]** on [Freenode][5] IRC

  [1]: https://elixir-lang.org/getting-started/introduction.html
  [2]: https://github.com/elixir-lang/elixir/issues
  [3]: https://groups.google.com/group/elixir-lang-core
  [4]: https://webchat.freenode.net/?channels=#elixir-lang
  [5]: http://www.freenode.net
  [6]: https://elixir-lang.org/docs.html
  [7]: CODE_OF_CONDUCT.md

## License

"Elixir" and the Elixir logo are copyright (c) 2012 Plataformatec.

Elixir source code is released under Apache 2 License.

Check [NOTICE](NOTICE) and [LICENSE](LICENSE) files for more
information.
