<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

<h1>
 <picture>
  <source media="(prefers-color-scheme: dark)" srcset="https://github.com/elixir-lang/elixir-lang.github.com/raw/main/images/logo/logo-dark.png">
  <img alt="Elixir logo" src="https://github.com/elixir-lang/elixir-lang.github.com/raw/main/images/logo/logo.png" width="200">
 </picture>
</h1>

[![CI](https://github.com/elixir-lang/elixir/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/elixir-lang/elixir/actions/workflows/ci.yml?query=branch%3Amain)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/10187/badge)](https://www.bestpractices.dev/projects/10187)

Elixir is a dynamic, functional language designed for building scalable
and maintainable applications.

For more about Elixir, installation and documentation,
[check Elixir's website](https://elixir-lang.org/).

## Policies

New releases are announced in the [announcement mailing list][8].
You can subscribe by sending an email to <elixir-lang-ann+subscribe@googlegroups.com>
and replying to the confirmation email.

All security releases [will be tagged with `[security]`][10]. For more
information, please read our [Security Policy][9].

All interactions in our official communication channels follow our
[Code of Conduct][1].

All contributions are required to conform to our [Open Source Policy][11].

## Bug reports

For reporting bugs, [visit our issue tracker][2] and follow the steps
for reporting a new issue. **Please disclose security vulnerabilities
privately at <elixir-security@googlegroups.com>**.

## Issues tracker management

All currently open bugs related to the Elixir repository are listed
in the issues tracker. The Elixir team uses the issues tracker to focus
on *actionable items*, including planned enhancements in the short and
medium term. We also do our best to label entries for clarity and to ease
collaboration.

Our *actionable item policy* has some important consequences, such as:

  * Proposing new features as well as requests for support, help, and
    guidance must be done in their own spaces, detailed next.

  * Issues we have identified to be outside of Elixir's scope,
    such as an upstream bug, will be closed (and requested to be moved
    elsewhere if appropriate).

  * We actively close unrelated and non-actionable issues to keep the
    issues tracker tidy. We may get things wrong from time to
    time and will gladly revisit issues, reopening when necessary.

Keep the tone positive and be kind! For more information, see the
[Code of Conduct][1].

### Proposing new features

For proposing new features, please start a discussion in the
[Elixir Core mailing list][3]. The [language development history and
its focus are described on our website](https://elixir-lang.org/development.html).

Keep in mind that it is your responsibility to argue and explain
why a feature is useful and how it will impact the codebase and
the community. A good proposal includes the problem description
and how the proposed solution compares with existing alternatives
in the Elixir ecosystem (as well as in other languages). To iron
out a proposal before submission, consider using and gathering
feedback from the community spaces [listed on the sidebar of the
Elixir website](https://elixir-lang.org/).

Once a proposal is accepted, it will be added to [the issue tracker][2].
Features and bug fixes that have already been merged and will be included
in the next release are then "closed" and added to the [changelog][7].

### Discussions, support, and help

For general discussions, support, and help, please use the community
spaces [listed on the sidebar of the Elixir website](https://elixir-lang.org/),
such as forums, chat platforms, etc, where the wider community will be available
to help you.

## Compiling from source

For the many different ways to install Elixir,
[see our installation instructions on the website](https://elixir-lang.org/install.html).
However, if you want to contribute to Elixir, you will need to compile from source.

First, [install Erlang](https://elixir-lang.org/install.html#installing-erlang).
After that, clone this repository to your machine, compile and test it:

```sh
git clone https://github.com/elixir-lang/elixir.git
cd elixir
make
```

> Note: if you are running on Windows,
[this article includes important notes for compiling Elixir from source
on Windows](https://github.com/elixir-lang/elixir/wiki/Windows).

In case you want to use this Elixir version as your system version,
you need to add the `bin` directory to [your PATH environment variable](https://elixir-lang.org/install.html#setting-path-environment-variable).

When updating the repository, you may want to run `make clean` before
recompiling. For deterministic builds, you should set the environment
variable `ERL_COMPILER_OPTIONS=deterministic`.

## Contributing

Contributions to Elixir are always welcome! Before you get started, please check
out our [CONTRIBUTING.md](CONTRIBUTING.md) file. There you will find detailed
guidelines on how to set up your environment, run the test suite, format your
code, and submit pull requests. We also include information on our review
process, licensing requirements, and helpful tips to ensure a smooth
contribution experience.

## Development links

  * [Elixir Documentation][6]
  * [Elixir Core Mailing list (development)][3]
  * [Announcement mailing list][8]
  * [Code of Conduct][1]
  * [Issue tracker][2]
  * [Changelog][7]
  * [Security Policy][9]
  * **[#elixir][4]** on [Libera.Chat][5] IRC

  [1]: CODE_OF_CONDUCT.md
  [2]: https://github.com/elixir-lang/elixir/issues
  [3]: https://groups.google.com/group/elixir-lang-core
  [4]: https://web.libera.chat/#elixir
  [5]: https://libera.chat
  [6]: https://elixir-lang.org/docs.html
  [7]: CHANGELOG.md
  [8]: https://groups.google.com/group/elixir-lang-ann
  [9]: SECURITY.md
  [10]: https://groups.google.com/forum/#!searchin/elixir-lang-ann/%5Bsecurity%5D%7Csort:date
  [11]: OPEN_SOURCE_POLICY.md

## License

"Elixir" and the Elixir logo are registered trademarks of The Elixir Team.

Elixir source code is released under Apache License 2.0.

Check [LICENSE](LICENSE) file for more information.
