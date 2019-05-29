![Elixir](https://github.com/elixir-lang/elixir-lang.github.com/raw/master/images/logo/logo.png)
=========
[![Travis build](https://secure.travis-ci.org/elixir-lang/elixir.svg?branch=master
"Build Status")](https://travis-ci.org/elixir-lang/elixir)
[![Windows build](https://ci.appveyor.com/api/projects/status/macwuxq7aiiv61g1?svg=true)](https://ci.appveyor.com/project/josevalim/elixir)

Elixir is a dynamic, functional language designed for building scalable
and maintainable applications.

For more about Elixir, installation and documentation,
[check Elixir's website](https://elixir-lang.org/).

## Policies

New releases are announced in the [announcement mailing list][8].
You can subscribe by sending an email to elixir-lang-ann+subscribe@googlegroups.com and replying to the confirmation email.

All security releases [will be tagged with `[security]`][10]. For more information, please read our [Security Policy][9].

All interactions in our official communication channels follow our [Code of Conduct][1].

## Bug reports

For reporting bugs, [visit our issue tracker][2] and follow the steps
for reporting a new issue. **Please disclose security vulnerabilities
privately at elixir-security@googlegroups.com**.

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

Please see the [CONTRIBUTING](CONTRIBUTING.md) file for detailed information on how to contribute.

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
