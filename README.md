# Elixir [![Build Status](https://secure.travis-ci.org/josevalim/elixir.png "Build Status")](http://travis-ci.org/josevalim/elixir)

Elixir is a programming language built on top of Erlang. As Erlang, it is a functional language with strict evaluation and dynamic typing built to support distributed, fault-tolerant, non-stop applications with hot swapping.

The main difference between Elixir and Erlang is its more natural homoiconic syntax that supports meta-programming. Elixir also supports polymorphism via protocols (similar to Clojure's), dynamic records and a reference mechanism.

Elixir and Erlang shares the same bytecode and data types. This means you can invoke Erlang code from Elixir (and vice-versa) without any ceremony or performance hit.

# Usage

Elixir is still in development but ready to try out! First, you need to
clone this repository to your machine, compile and test it:

    $ git clone https://github.com/josevalim/elixir.git
    $ cd elixir
    $ make test

    $ bin/elixir -v
    Elixir 0.4.0.dev

If tests pass, you are ready to move on to the [Getting Started guide](https://github.com/josevalim/elixir/blob/master/docs/1_getting_started.md) or to try Interactive Elixir by running: `bin/iex` in your terminal.

However, if tests fail, it is likely you have an outdated Erlang version (Elixir requires Erlang R14B03 or later). You can check your Erlang version by calling `erl` in the command line. You will see some information as follow:

    Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

If you have the correct version and tests still fail, feel free to open an issue [in the issues tracker on Github](https://github.com/josevalim/elixir/issues).

# Contributing & Roadmap

If you want to contribute, the code is organized as follow:

* `include`, `src` - Both directories contain part of the source code written in Erlang. `yecc` is used as the parser;

* `lib` - Contains Elixir's STDLIB, written in Elixir;

* `test/elixir` - Tests for Elixir's STDLIB, written in Elixir. For this purpose, Elixir ships with a small unit test library called `ExUnit`;

* `test/erlang` - Contains tests for Elixir, written in Erlang. Usually, just internal stuff is tested here. The preferred way to test is in Elixir itself.

We usually keep a list of features and bugs (in the issues tracker)[https://github.com/josevalim/elixir/issues].

# Important links

* #elixir-lang on freenode IRC
* [Getting Started Guide](https://github.com/josevalim/elixir/blob/master/docs/1_getting_started.md)
* [Mailing list](http://groups.google.com/group/elixir-lang-core)
* [Issues tracker](https://github.com/josevalim/elixir/issues)
* [Textmate Bundle for Elixir](https://github.com/josevalim/elixir-tmbundle)

# License

Copyright (c) 2011-2012 José Valim, jose.valim@gmail.com

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.