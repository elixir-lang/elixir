# 3 Modules

In Elixir, you can group several functions into a module. In previous chapter, we have invoked for example functions from the module List:

    iex> List.flatten [1,[2],3]
    [1, 2, 3]

In order to create our own modules in Elixir, all we have to do is to call the `defmodule` function and use `def` to define our functions:

    iex> defmodule Math do
    ...>   def sum(a, b) do
    ...>     a + b
    ...>   end
    ...> end

    iex> Math.sum(1, 2)
    3

## 3.1 Compilation

Most of the times it is convenient to write modules into files so they can be compiled and re-used. Let's assume we have a file named `math.ex` with the following contents:

    defmodule Math do
      def sum(a, b) do
        a + b
      end
    end

This file can be compiled using `bin/elixirc`:

    bin/elixirc math.ex

Which will then generate a file named `::Math.beam` containing the byte code for the defined module. Now, if we start `bin/iex` again, our module definition will be available (considering `bin/iex` is being started in the same directory the byte code file is):

    iex> Math.sum(1, 2)
    3

Elixir projects are usually organized into three directories:

* ebin - holds the compiled bytecode
* lib - holds elixir code (usually `.ex` files)
* test - holds tests (usually `.exs` files)

In such cases, since the byte code is in `ebin`, you need to explicitly tell Elixir to lookup for code in the `ebin` directory:

    bin/iex -pa ebin

Where `-pa` stands for `path append`. The same option can also be passed to `elixir` and `elixirc` executables. You can execute `bin/elixir` and `bin/elixirc` to get a full list of options.

## 3.2 Scripted mode

Besides the Elixir file `.ex`, Elixir also supports `.exs` files for scripting. Elixir treats both files exactly the same way, the only difference is intention. `.ex` files are meant to be compiled while `.exs` files are used for scripting, without a need for compilation. For instance, one can create a file called `math.exs`:

    defmodule Math do
      def sum(a, b) do
        a + b
      end
    end

    IO.puts Math.sum(1, 2)

And execute it as:

    bin/elixir math.exs

The file will be compiled in memory and executed, printing 3 as result. No byte-code file will be created.

## 3.3 Functions, privates and macros

There are many definitions available inside Elixir modules. They are:

* `def`  - defines a function;
* `defp` - defines a private function;
* `defmacro` - defines a macro;
* `defrecord` - defines a record;
* `defprotocol` - defines a protocol;
* `defimpl` - defines an implementation for a protocol

## 3.4 Directives

In order to support software-reuse, Elixir supports four directives:

### 3.4.1 import

You must use `import` whenever you want to easily access functions from others modules without using the qualified name. For instance, if you want to use the `values` function from `Orddict` several times in your module and you don't want to always type `Orddict.values`, you can simply import it:

    defmodule Math do
      import Orddict, only: [values: 1]

      def some_function do
        # call values(orddict)
      end
    end

In this case, we are importing only the function `values` (with arity 1) from `Orddict`. Although `only` is optional, its usage is recommended. `except` could also be given as an option.

This mechanism cannot be used to import macros. Only functions.

### 3.4.2 refer

`refer` is responsible to setup references aliases for a given module. For instance, one can do:

    defmodule Math do
      refer MyOrddict, as: Orddict
    end

And now, any reference to `Orddict` will be automatically replaced by `MyOrddict`. In case one wants to access the original `Orddict`, it can be done by prefixing the module name with `::`:

    Orddict.values   #=> uses ::MyOrddict.values
    ::Orddict.values #=> uses ::Orddict.values

### 3.4.3 require

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

### 3.4.4 use

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

## 3.5 Module nesting

In Elixir, nesting a module inside the other does not affect the its name:

    defmodule Foo do
      defmodule Bar do
      end
    end

The example above will define two modules `Foo` and `Bar`. Notice that the second module is **not** called `Foo::Bar`. In general, nesting modules is discouraged in Elixir.

## 3.6 References

[Chapter 2: Dipping the toes](https://github.com/josevalim/elixir/blob/master/docs/2_dipping_the_toes.md) | [Index](https://github.com/josevalim/elixir/blob/master/docs/0_index.md) | [Chapter 4: Protocols & Records](https://github.com/josevalim/elixir/blob/master/docs/4_protocols_and_records.md)