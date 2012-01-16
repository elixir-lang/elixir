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
