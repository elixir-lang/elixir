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

If tests pass, you are ready to try Interactive Elixir by running: `bin/iex` in your terminal.

However, if tests fail, it is likely you have an outdated Erlang version. You can check your Erlang version by calling `erl` in the command line. You will see some information as follow:

    Erlang R14B03 (erts-5.8.4) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Elixir requires Erlang R14B03 or later. If you have the correct version and tests still fail, feel free to open an issue in the issues tracker on Github. If all tests pass, you are ready to go.

## Contributing & Roadmap

If you want to contribute, the code is organized as follow:

* `include`, `src` - Both directories contain part of the source code written in Erlang. `yecc` is used as the parser;

* `lib` - Contains Elixir's STDLIB, written in Elixir;

* `test/elixir` - Tests for Elixir's STDLIB, written in Elixir. For this purpose, Elixir ships with a small unit test library called `ExUnit`;

* `test/erlang` - Contains tests for Elixir, written in Erlang. Usually, just internal stuff is tested here. The preferred way to test is in Elixir itself.

## Important links

* [Mailing list](http://groups.google.com/group/elixir-lang-core)
* #elixir-lang on freenode IRC
* [Textmate Bundle for Elixir](https://github.com/josevalim/elixir-tmbundle)

## Modules

In order to create a new module in Elixir, all we have to do is to call the `defmodule` macro passing its contents:

    defmodule Math do
      def sum(a, b) do
        a + b
      end
    end

    Math.sum(1, 2) #=> 3

There are many definitions available inside Elixir modules. They are:

* `def`  - defines a function;
* `defp` - defines a private function;
* `defmacro` - defines a macro;
* `defrecord` - defines a record;
* `defprotocol` - defines a protocol;
* `defimpl` - defines an implementation for a protocol

All those definitions will be described with the detail throughout this tutorial (coming soon).

### Module nesting

In Elixir, nesting a module inside the other does not affect the its name:

    defmodule Foo do
      defmodule Bar do
      end
    end

The example above will define two modules `Foo` and `Bar`. Notice that the second module is **not** called `Foo::Bar`. In general, nesting modules is discouraged in Elixir.

### Directives

In order to support software-reuse, Elixir supports four directives:

#### import

You must use `import` whenever you want to easily access functions from others modules without using the qualified name. For instance, if you want to use the `values` function from `Orddict` several times in your module and you don't want to always type `Orddict.values`, you can simply import it:

    defmodule Math do
      import Orddict, only: [values: 1]

      def some_function do
        # call values(orddict)
      end
    end

In this case, we are importing only the function `values` (with arity 1) from `Orddict`. Although `only` is optional, its usage is recommended. `except` could also be given as an option.

This mechanism cannot be used to import macros. Only functions.

#### refer

`refer` is responsible to setup references aliases for a given module. For instance, one can do:

    defmodule Math do
      refer MyOrddict, as: Orddict
    end

And now, any reference to `Orddict` will be automatically replaced by `MyOrddict`. In case one wants to access the original `Orddict`, it can be done by prefixing the module name with `::`:

    Orddict.values   #=> uses ::MyOrddict.values
    ::Orddict.values #=> uses ::Orddict.values

#### require

`require` allows you to enable the given module macros. For instance, suppose you created your own `if` implementation called in the module `MyMacros`. If you want to invoke it, you need to first explicitly require the `MyMacros`:

    defmodule Math do
      require MyMacros
      MyMacros.if do_something, it_works
    end

An attempt to call a macro that was not loaded will raise an error. It is important to note that `require` is the only directive that is **lexical**. This means you can require specific macros inside specific functions:

    defmodule Math do
      def some_function do
        require MyMacros, import: true
        if do_something, it_works
      end
    end

In the example above, we required and imported macros from `MyMacro`, replacing the original `if` implementation by our own during that specific function. All other functions in that module will still be able to use the original one.

Finally, `require` also accepts `only` and `except` as options to select which macros to import. Consecutive calls to `require` passing the same models override previous definitions.

    defmodule MyIo
      # Import bit-or and bit-and from Bitwise
      require Bitwise, only: [bor: 2, band: 2]
      def some_func(x, y, z), do: x bor y band z

      # Import all, except bxor, overriding previous
      require Bitwise, except: [bxor: 2]
    end

You can read more about creating your own macros in the "Meta-programming in Elixir" section.

#### use

`use` is the simplest mechanism of all three as it simply intends to be a common API for extension. For instance, in order to use `ExUnit` test framework, you simply need to use `ExUnit::Case` in your module:

    defmodule AssertionTest do
      use ExUnit::Case

      def test_always_pass do
        true = true
      end
    end

By calling `use`, a hook called `__using__` will be invoked in `ExUnit::Case` which will then do the proper setup. In other words, `use` is simply a translation to:

    defmodule AssertionTest do
      require ExUnit::Case
      ExUnit::Case.__using__(:"::AssertionTest")

      def test_always_pass do
        true = true
      end
    end

## Meta-programming in Elixir

Elixir is an homoiconic language. Any Elixir program can be represented using its own data structures. This section describes the Elixir language specification for such data structures.

The building block of Elixir homoiconicity is a tuple with three elements, for example:

    { :sum, 1, [1, 2, 3] }

The tuple above represents a function call to sum passing 1, 2 and 3 as arguments. The tuple elements are:

* The first element of the tuple is always an atom or another tuple in the same representation;
* The second element of the tuple is always an integer representing the line number;
* The third element of the tuple are the arguments for the function call. The third argument may also be an atom (nil or quoted), meaning that it may be a variable call.

You can get the representation of any expression by using the quote macro:

    quote { sum(1, 2, 3) }
    #=> { :sum, 0, [1, 2, 3] }

Besides the tuple, Elixir has a few literals. Literals are data types that when quoted return themselves. They are:

    :sum         #=> Atoms
    1            #=> Integers
    2.0          #=> Floats
    [1,2]        #=> Lists
    "binaries"   #=> Binaries
    {key, value} #=> Key-value pairs (i.e. a tuple with two elements)

With those basic structures in mind, we are ready to define our own macro.

### Defining your own macro

A macro can be define using `defmacro`. For instance, we can define a macro called unless which works the same as Ruby's unless in just few lines of code:

    defmacro unless(clause, options) do
      quote { if(!unquote(clause), unquote(options)) }
    end

In the example above, unless will be called receiving two arguments: a `clause` and `options`. However, note that unless won't receive its values, but its expressions. For example, if one calls:

    unless 2 + 2 == 5, do: call_function()

Our `unless` macro will receive the following:

    unless({:==, 1, [{:+, 1, [2, 2]}, 5]}, { :call_function, 1, [] })

After being invoked, our `unless` macro will call `quote`, to return a tree representation of the `if` clause. This means we are transforming our `unless` in a `if`!

However, there is a common mistake when quoting expressions which is that developers usually forget to `unquote` the proper expression. In order to understand what `unquote` does, let's simply remove it:

    defmacro unless(clause, options) do
      quote { if(!clause, options) }
    end

When called as `unless 2 + 2 == 5, do: call_function()`, our `unless` would then return:

    { :if, 0, [{ :!, 0, [{:custom, 0, quoted}]}, do: {:options, 0, quoted}] }

Notice that the tree structure returned by unless is trying to access `custom` and `options` as variables instead of using the `2 + 2 == 5` and `call_function()` expressions we passed as parameters. This is because we forgot to unquote! If we add unquote back:

    defmacro unless(clause, options) do
      quote { if(!unquote(clause), unquote(options)) }
    end

It would return:

    { :if, 0, [{ :!, 0, [{:==, 1, [{:+, 1, [2, 2]}, 5]}]},
      do: { :call_function, 1, [] }] }

In other words, unquote is a mechanism to inject expressions into the tree being quoted and is essential to the meta-programming mechanism. Elixir also provides `unquote_splicing`.

### Locals and macros

In order to support recursion, macros cannot be called locally. For example, one cannot write:

    defmodule MyMacros
      defmacro delegate([h|t], to: target) do
        # ...
      end

      # Call the macro delegate just defined above
      delegate [values: 1], to: List
    end

In order to access the macro, it needs to be defined in an outer module:

    defmodule MyMacros::Support
      defmacro delegate([h|t], to: target) do
        # ...
      end
    end

    defmodule MyMacros
      require MyMacros::Support, import: true
      delegate [values: 1], to: List
    end

## Performance

### Compilation to Native Code

Elixir can compile to native code using the Hipe compiler. All you need to do is to export the following before running your code:

    export ERL_COMPILER_OPTIONS=native

# License

See MIT-LICENSE attached.