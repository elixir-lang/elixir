# Elixir [![Build Status](https://secure.travis-ci.org/elixir-lang/elixir.png "Build Status")](http://travis-ci.org/josevalim/elixir)

Elixir is a programming language built on top of Erlang. As Erlang, it is a functional language with strict evaluation and dynamic typing built to support distributed, fault-tolerant, non-stop applications with hot swapping.

The main difference between Elixir and Erlang is its more natural homoiconic syntax that supports meta-programming. Elixir also supports polymorphism via protocols (similar to Clojure's), dynamic records and a reference mechanism.

Elixir and Erlang share the same bytecode and data types. This means you can invoke Erlang code from Elixir (and vice-versa) without any conversion or performance hit.

# Usage

You can run and learn about Elixir in the [Getting Started guide](https://github.com/elixir-lang/elixir/blob/master/docs/0_index.md).

But if you just want to compile and try Elixir, with no compromises, you can clone this repository to your machine, compile and test it:

    $ git clone https://github.com/elixir-lang/elixir.git
    $ cd elixir
    $ make test

    $ bin/elixir -v
    Elixir 0.4.0.dev

If tests pass, you are ready to move on to the [Getting Started guide](https://github.com/elixir-lang/elixir/blob/master/docs/0_index.md) or to try Interactive Elixir by running: `bin/iex` in your terminal.

However, if tests fail, it is likely you have an outdated Erlang version (Elixir requires Erlang R15B or later). You can check your Erlang version by calling `erl` in the command line. You will see some information as follow:

    Erlang R15B (erts-5.8.4) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

If you have the correct version and tests still fail, feel free to open an issue [in the issues tracker on Github](https://github.com/elixir-lang/elixir/issues).

# Contributing & Roadmap

If you want to contribute, the code is organized as follow:

* `include`, `src` - Both directories contain part of the source code written in Erlang. `yecc` is used as the parser;

* `lib` - Contains Elixir's STDLIB, written in Elixir;

* `test/elixir` - Tests for Elixir's STDLIB, written in Elixir. For this purpose, Elixir ships with a small unit test library called `ExUnit`;

* `test/erlang` - Contains tests for Elixir, written in Erlang. Usually, just internal stuff is tested here. The preferred way to test is in Elixir itself.

We usually keep a list of features and bugs [in the issues tracker](https://github.com/elixir-lang/elixir/issues).

# Important links

* #elixir-lang on freenode IRC
* [Getting Started Guide](https://github.com/elixir-lang/elixir/blob/master/docs/0_index.md)
* [Mailing list](http://groups.google.com/group/elixir-lang-core)
* [Issues tracker](https://github.com/elixir-lang/elixir/issues)
* [Textmate Bundle for Elixir](https://github.com/elixir-lang/elixir-tmbundle)

# License

Copyright (c) 2012 Plataformatec. See LICENSE file.