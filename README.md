# Elixir [![Build Status](https://secure.travis-ci.org/josevalim/elixir.png "Build Status")](http://travis-ci.org/josevalim/elixir)

Elixir is a programming language built on top of Erlang. As Erlang, it is a functional language with strict evaluation and dynamic typing built to support distributed, fault-tolerant, non-stop applications with hot swapping.

The main difference between Elixir and Erlang is its more natural homoiconic syntax that allows meta-programming. Support for more features like polymorphism, import mechanisms is upcoming.

Elixir and Erlang shares the same bytecode and data types. This means you can invoke Erlang code from Elixir (and vice-versa) without any performance hit.

# Usage

Elixir is still in development but ready to try out! First, you need to
clone this repository to your machine, compile and test it:

    $ git clone https://github.com/josevalim/elixir.git
    $ cd elixir
    $ make test

    $ bin/elixir -v
    Elixir 0.4.0.dev

If tests pass, you are ready to try Interactive Elixir by running: `bin/iex`.

However, if tests fail, it is likely you have an outdated Erlang version. You can check your Erlang version by calling `erl` in the command line. You will see some information as follow:

    Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Elixir requires Erlang R14B03 or later. If you have the correct version and tests still fail, feel free to open an issue in the issues tracker on Github. If all tests pass, you are ready to go.

## Contributing & Roadmap

If you want to contribute, first you need to clone this repository to your machine, compile and run the tests:

    $ git clone https://github.com/josevalim/elixir.git
    $ cd elixir
    $ make test

If tests fail, it is likely you have an outdated Erlang version. You can check your Erlang version by calling `erl` in the command line. You will see some information as follow:

    Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Elixir requires Erlang R14B03 or later. If you have the correct version and tests still fail, feel free to open an issue in the issues tracker on Github. If all tests pass, you are ready to go.

The code is organized as follow:

* `include`, `src` - Both directories contain part of the source code written in Erlang. `yecc` is used as the parser;

* `lib` - Contains Elixir's STDLIB, written in Elixir;

* `test/elixir` - Tests for Elixir's STDLIB, written in Elixir. For this purpose, Elixir ships with a small unit test library called `ExUnit`;

* `test/erlang` - Contains tests for Elixir, written in Erlang. Usually, just internal stuff is tested here. The preferred way to test is in Elixir itself.

If you are interested, check out the ROADMAP.md file in the repository or keep reading this README to find items to be implemented.

## Important links

* [Mailing list](http://groups.google.com/group/elixir-lang-core)
* #elixir-lang on freenode IRC
* [Textmate Bundle for Elixir](https://github.com/josevalim/elixir-tmbundle)

## Meta-programming in Elixir

Elixir is an homoiconic language. Any Elixir program can be represented using its own data structures. This section describes the Elixir language specification for such data structures.

The building block of Elixir homoiconicity is a tuple with three elements, for example:

    { :sum, 1, [1, 2, 3] }

The tuple above represents a function call to sum passing 1, 2 and 3 as arguments. The tuple elements are:

* The first element of the tuple is always an atom or another tuple in the same representation;
* The second element of the tuple is always an integer representing the line number;
* The third element of the tuple are the arguments for the function call. The third argument may also be false, meaning that it represents either a variable or a function call. It is up to Elixir interpreters (or your own macros) to decide it.

You can get the representation of any expression by using the quote macro:

    quote sum(1, 2, 3)
    #=> { :sum, 0, [1, 2, 3] }

Besides the tuple, Elixir has a few literals. Literals are elements that when quoted return themselves. They are:

    :sum         #=> Atoms
    1            #=> Integers
    2.0          #=> Floats
    [1,2]        #=> Lists
    {key, value} #=> Key-value pairs (i.e. a tuple with two elements)

Parenthesis in Elixir are used to group a list of expressions in a block macro:

    quote((
      1
      2
      3
    ))
    #=> { :block, 0, [1,2,3] }

## Performance

### Compilation to Native Code

Elixir can compile to native code using the Hipe compiler. All you need to do is to export the following before running your code:

    export ERL_COMPILER_OPTIONS=native

# License

See MIT-LICENSE attached.