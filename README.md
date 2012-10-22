![Elixir](https://github.com/elixir-lang/elixir-lang.github.com/raw/master/images/logo/logo.png)
=========
[![Build Status](https://secure.travis-ci.org/elixir-lang/elixir.png?branch=master "Build Status")](http://travis-ci.org/elixir-lang/elixir)

For more about Elixir, installation and documentation, [check Elixir's website](http://elixir-lang.org/).

# Usage

If you want to contribute to Elixir or run it from source, clone this repository to your machine, compile and test it:

    $ git clone https://github.com/elixir-lang/elixir.git
    $ cd elixir
    $ make test

If tests pass, you are ready to move on to the [Getting Started guide][1] or to try Interactive Elixir by running: `bin/iex` in your terminal.

However, if tests fail, it is likely you have an outdated Erlang version (Elixir requires Erlang R15B or later). You can check your Erlang version by calling `erl` in the command line. You will see some information as follow:

    Erlang R15B (erts-5.8.4) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

If you have the correct version and tests still fail, feel free to [open an issue][2].

# Contributing

If you want to contribute, Elixir code is divided in applications inside the `lib` folder:

* `elixir` - Contains Elixir's kernel and stdlib;

* `eex` - Template engine that allows you to embed Elixir;

* `ex_unit` - Simple test framework that ships with Elixir;

* `iex` — IEx, Elixir's interactive shell

* `mix` — Elixir's build tool

We usually keep a list of features and bugs [in the issue tracker][2].

# Important links

* #elixir-lang on freenode IRC
* [Website][1]
* [Issue tracker][2]
* [Mailing list][3]

  [1]: http://elixir-lang.org
  [2]: https://github.com/elixir-lang/elixir/issues
  [3]: http://groups.google.com/group/elixir-lang-core

# License

"Elixir" and the Elixir logo are copyright (c) 2012 Plataformatec.

Elixir source code is released under Apache 2 License with some parts under Erlang's license (EPL).

Check LEGAL and LICENSE files for more information.