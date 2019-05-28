![Elixir](https://github.com/elixir-lang/elixir-lang.github.com/raw/master/images/logo/logo.png)
=========
[![Travis build](https://secure.travis-ci.org/elixir-lang/elixir.svg?branch=master
"Build Status")](https://travis-ci.org/elixir-lang/elixir)
[![Windows build](https://ci.appveyor.com/api/projects/status/macwuxq7aiiv61g1?svg=true)](https://ci.appveyor.com/project/josevalim/elixir)

Elixir is a dynamic, functional language designed for building scalable
and maintainable applications.

For more about Elixir, installation and documentation,
[check Elixir's website](https://elixir-lang.org/).

## Announcements

New releases are announced in the
[announcement mailing list][8].
You can subscribe by sending an email to elixir-lang-ann+subscribe@googlegroups.com and replying to the confirmation email.

All security releases [will be tagged with `[security]`][10]. For more information, please read our [Security Policy][9]

## Compiling from source

For the many different ways to install Elixir,
[see our installation instructions on the website](https://elixir-lang.org/install.html).
To compile from source, you can follow the steps below.

First, [install Erlang](https://elixir-lang.org/install.html#installing-erlang). Then clone this repository to your machine, compile and test it:

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

If tests pass, you can use Interactive Elixir by running `bin/iex` in your terminal.

However, if tests fail, it is likely that you have an outdated Erlang/OTP version
(Elixir requires Erlang/OTP 20.0 or later). You can check your Erlang/OTP version
by calling `erl` in the command line. You will see some information as follows:

    Erlang/OTP 20 [erts-9.0] [smp:2:2] [async-threads:10] [kernel-poll:false]

If you have properly set up your dependencies and tests still fail,
you may want to open up a bug report, as explained next.

## Official communication channels

All interactions in our official communication channels follow our [Code of Conduct][1]:

* The [official GitHub projects](https://github.com/elixir-lang/) and code reviews.
* The official elixir-lang mailing lists.
* The **[#elixir-lang][4]** IRC channel on [Freenode][5].

Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by emailing: elixir-lang-conduct@googlegroups.com

## Bug reports

For reporting bugs, [visit our issue tracker][2] and follow the steps
for reporting a new issue. **Please disclose security vulnerabilities
privately at elixir-security@googlegroups.com**.

## Proposing new features

For proposing new features, please start a discussion in the
[Elixir Core mailing list][3]. Keep in mind that it is your responsibility
to argue and explain why a feature is useful and how it will impact the
codebase and the community.

Once a proposal is accepted, it will be added to [the issue tracker][2].
The issue tracker focuses on *actionable items* and it holds a list of
upcoming enhancements and pending bugs. All entries in the tracker are
tagged for clarity and to ease collaboration.

Features and bug fixes that have already been merged and will be included
in the next release are marked as "closed" in the issue tracker and are
added to the [changelog][7].

## Contributing

We welcome everyone to contribute to Elixir. To do so, there are a few
things you need to know about the code. First, Elixir code is divided
in applications inside the `lib` folder:

* `elixir` - Contains Elixir's kernel and standard library

* `eex` - EEx is the template engine that allows you to embed Elixir

* `ex_unit` - ExUnit is a simple test framework that ships with Elixir

* `iex` - IEx stands for Interactive Elixir: Elixir's interactive shell

* `logger` - Logger is the built-in logger

* `mix` - Mix is Elixir's build tool

You can run all tests in the root directory with `make test` and you can
also run tests for a specific framework `make test_#{APPLICATION}`, for example,
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
More tasks can be found by reading the [Makefile](Makefile).

With tests running and passing, you are ready to contribute to Elixir and
[send a pull request](https://help.github.com/articles/using-pull-requests/).
We have saved some excellent pull requests we have received in the past in
case you are looking for some examples:

* [Implement Enum.member? - Pull Request](https://github.com/elixir-lang/elixir/pull/992)
* [Add String.valid? - Pull Request](https://github.com/elixir-lang/elixir/pull/1058)
* [Implement capture_io for ExUnit - Pull Request](https://github.com/elixir-lang/elixir/pull/1059)

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
believe they should be merged without squashing, please mention it in
a comment.

## Building documentation

Building the documentation requires [ExDoc](https://github.com/elixir-lang/ex_doc)
to be installed and built alongside Elixir:

```sh
# After cloning and compiling Elixir, in its parent directory:
git clone git://github.com/elixir-lang/ex_doc.git
cd ex_doc && ../elixir/bin/mix do deps.get, compile
```

Now go back to Elixir's root directory and run:

```sh
make docs                  # to generate HTML pages
make docs DOCS_FORMAT=epub # to generate EPUB documents
```

This will produce documentation sets for `elixir`, `mix`, etc. under
the `doc` directory. If you are planning to contribute documentation,
[please check our best practices for writing documentation](https://hexdocs.pm/elixir/writing-documentation.html).

## Development links

  * [Elixir Documentation][6]
  * [Elixir Core Mailing list (development)][3]
  * [Announcement mailing list][8]
  * [Code of Conduct][1]
  * [Issue tracker][2]
  * [Changelog][7]
  * [Security Policy][9]
  * **[#elixir-lang][4]** on [Freenode][5] IRC

  [1]: CODE_OF_CONDUCT.md
  [2]: https://github.com/elixir-lang/elixir/issues
  [3]: https://groups.google.com/group/elixir-lang-core
  [4]: https://webchat.freenode.net/?channels=#elixir-lang
  [5]: https://www.freenode.net
  [6]: https://elixir-lang.org/docs.html
  [7]: CHANGELOG.md
  [8]: https://groups.google.com/group/elixir-lang-ann
  [9]: SECURITY.md
  [10]: https://groups.google.com/forum/#!searchin/elixir-lang-ann/%5Bsecurity%5D%7Csort:date

## License

"Elixir" and the Elixir logo are copyright (c) 2012 Plataformatec.

Elixir source code is released under Apache 2 License.

Check [NOTICE](NOTICE) and [LICENSE](LICENSE) files for more information.
