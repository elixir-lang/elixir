![Elixir](https://github.com/elixir-lang/elixir-lang.github.com/raw/master/images/logo/logo.png)
=========
[![Build Status](https://secure.travis-ci.org/elixir-lang/elixir.svg?branch=master "Build Status")](https://travis-ci.org/elixir-lang/elixir)

For more about Elixir, installation and documentation, [check Elixir's website](http://elixir-lang.org/).

## Usage

If you want to contribute to Elixir or run it from source, clone this repository to your machine, compile and test it:

```sh
git clone https://github.com/elixir-lang/elixir.git
cd elixir
make clean test
```

> Note: if you are running on Windows, [this article includes important notes for compiling Elixir from source on Windows](https://github.com/elixir-lang/elixir/wiki/Windows).

If Elixir fails to build (specifically when pulling in a new version via git), be sure to remove any previous build artifacts by running `make clean`, then `make test`.

If tests pass, you are ready to move on to the [Getting Started guide][1] or to try Interactive Elixir by running: `bin/iex` in your terminal.

However, if tests fail, it is likely you have an outdated Erlang version (Elixir requires Erlang 17.0 or later). You can check your Erlang version by calling `erl` in the command line. You will see some information as follows:

`Erlang/OTP 17 [erts-6.0] [source-07b8f44] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]`

If you have the correct version and tests still fail, feel free to [open an issue][2].

## Building documentation

Building the documentation requires [ex_doc](https://github.com/elixir-lang/ex_doc) to be installed and built in the same containing folder as elixir.

```sh
# After cloning and compiling Elixir
git clone git://github.com/elixir-lang/ex_doc.git
cd ex_doc && ../elixir/bin/mix do deps.get, compile
cd ../elixir && make docs
```

## Contributing

We appreciate any contribution to Elixir, so check out our [CONTRIBUTING.md](CONTRIBUTING.md) guide for more information. We usually keep a list of features and bugs [in the issue tracker][2].

## Important links

* [Elixir Website][1]
* [Elixir Documentation][7]
* **[#elixir-lang][5]** on [Freenode][6] IRC
* [Issue tracker][2]
* [elixir-talk Mailing list (questions)][3]
* [elixir-core Mailing list (development)][4]

  [1]: http://elixir-lang.org
  [2]: https://github.com/elixir-lang/elixir/issues
  [3]: https://groups.google.com/group/elixir-lang-talk
  [4]: https://groups.google.com/group/elixir-lang-core
  [5]: irc://chat.freenode.net/elixir-lang
  [6]: http://www.freenode.net/
  [7]: http://elixir-lang.org/docs.html

## License

"Elixir" and the Elixir logo are copyright (c) 2012 Plataformatec.

Elixir source code is released under Apache 2 License with some parts under Erlang's license (EPL).

Check [LEGAL](LEGAL) and [LICENSE](LICENSE) files for more information.
