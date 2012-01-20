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

Before diving into modules, let's first have a quick talk about compilation.

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

## 3.3 Functions and private functions

Inside a module, we can define functions (with `def`) and private function (with `defp`). A function defined with `def` is available to be invoked from other modules while a private function can only be invoked locally.

    defmodule Math do
      def sum(a, b) do
        do_sum(a, b)
      end

      defp do_sum(a, b) do
        a + b
      end
    end

    Math.sum(1, 2)    #=> 3
    Math.do_sum(1, 2) #=> error :undef

Function declarations also supports guards and multiple clauses. If a function has several clauses, Elixir will try each clause until find one that matches. For example, here is the implementation for a function that checks if the given number is zero or not:

    defmodule Math do
      def zero?(0) do
        true
      end

      def zero?(x) when is_number(x) do
        false
      end
    end

    Math.zero?(0)  #=> true
    Math.zero?(1)  #=> false

    Math.zero?([1,2,3])
    #=> error :function_clause

Notice that giving an argument that does not match any of the clauses raises an error. This mechanism of matching different functions of a clause is also the preferred way to do recursion in Elixir. A simple example that multiples each element in an list by two could be done as follow:

    defmodule List::Multiplier do
      def by_2([h|t]) do
        [ h * 2 | by_2(t) ]
      end

      def by_2([]) do
        []
      end
    end

In the example above, we recursively multiply each element of the list until the list is empty. With this example, we finish this section and now we are going to move on to module directives.

## 3.4 Directives

In order to support software-reuse, Elixir supports four directives:

### 3.4.1 import

We use `import` whenever we want to easily access functions from others modules without using the qualified name. For instance, if we want to use the `values` function from `Orddict` several times in a module and we don't want to always type `Orddict.values`, we can simply import it:

    defmodule Math do
      import Orddict, only: [values: 1]

      def some_function do
        # call values(orddict)
      end
    end

In this case, we are importing only the function `values` (with arity 1) from `Orddict`. Although `only:` is optional, its usage is recommended. `except` could also be given as an option.

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

`require` allows us to enable macros from a given module. For instance, suppose we created our own `my_if` implementation in a module named `MyMacros`. If we want to invoke it, we need to first explicitly require `MyMacros`:

    defmodule Math do
      require MyMacros
      MyMacros.my_if do_something, it_works
    end

An attempt to call a macro that was not loaded will raise an error. It is important to note that `require` and `refer` are the only directives that are **lexical**. This means we can require specific macros inside specific functions:

    defmodule Math do
      def some_function do
        require MyMacros, import: true
        my_if do_something, it_works
      end
    end

In the example above, we required and imported macros from `MyMacro` during that specific function. `my_if` won't be available in any other functions in that module.

Finally, `require` also accepts `only` and `except` as options to select which macros to import. Consecutive calls to `require` passing the same models override previous definitions.

    defmodule MyIo
      # Import bit-or and bit-and from Bitwise
      require Bitwise, only: [bor: 2, band: 2]
      def some_func(x, y, z), do: x bor y band z

      # Import all, except bxor, overriding previous call
      require Bitwise, except: [bxor: 2]
    end

We are going to discuss the creation of macros in chapter 5.

### 3.4.4 use

`use` is the simplest directive of all four as it simply intends to be a common API for extension. For instance, in order to use the `ExUnit` test framework that ships with Elixir, you simply need to use `ExUnit::Case` in your module:

    defmodule AssertionTest do
      use ExUnit::Case

      def test_always_pass do
        true = true
      end
    end

By calling `use`, a hook called `__using__` will be invoked in `ExUnit::Case` which will then do the proper setup. In other words, `use` is simply a translation to:

    defmodule AssertionTest do
      require ExUnit::Case
      ExUnit::Case.__using__(::AssertionTest)

      def test_always_pass do
        true = true
      end
    end

## 3.5 Module data

Elixir also allows module to store their own data. The canonical example for such data is annotating that a module implements the OTP behavior called `gen_server`:

    defmodule MyServer do
      @behavior :gen_server
      # ... callbacks ...
    end

Now if the module above does not implement any of the callbacks required by `gen_server`, a warning will be raised. Another data used internally by Elixir is is the `@vsn`:

    defmodule MyServer do
      @vsn 2
    end

`@vsn` refers to version is used by the code reloading mechanism to check if a module is updated or not. If no version is specified, the version is set to the MD5 of the module functions. Elixir has a handful of reserved data attributes. The following are currently functional in Elixir:

* `@behaviour` and `@behavior` - used for specifying an OTP or user-defined behavior;
* `@vsn` - used for specifying the module version;
* `@visibility` - sets the visibility for the following functions (accepts :public or :private as argument);

The following are also reserved by Elixir (as they have special semantics to the Erlang VM) but not currently supported (if you need support to any of these in your current projects, please make yourself heard in the issues tracker):

* `@spec` - provides an specification for the next function to be defined;
* `@callback` - provides an specification for the behavior callback;
* `@compile` - provides options for the module compilation;
* `@type` - provides a type to be used in @spec;
* `@export_type` - provides a type to be used in @spec that can be accessed from external specs;

Besides the built-in data above, any developer can also add custom data:

    defmodule MyServer do
      @my_data 13
      IO.puts @my_data #=> 13
    end

After the module is compiled, the stored custom data can be accessed via `__info__(:data)` and it will return an `Orddict`:

    MyServer.__info__(:data) #=> [my_data: 13]

Setting a data to nil automatically discards it from the `Orddict`.

## 3.6 Module nesting

In Elixir, nesting a module inside the other does not affect the its name:

    defmodule Foo do
      defmodule Bar do
      end
    end

The example above will define two modules `Foo` and `Bar`. Notice that the second module is **not** called `Foo::Bar`. In general, nesting modules is discouraged in Elixir.

## 3.7 References

In Erlang (and consequently in the Erlang VM), modules and functions are represented by atoms. For instance, this is valid Erlang code:

    Mod = lists,
    Mod:flatten([1,[2],3]).

In the example above, we store the atom `lists` in the variable `Mod` and then invoked the function flatten in it. In Elixir, exactly the same idiom is allowed. In fact, we could call the same function `flatten` in `lists` as:

    iex> :lists.flatten([1,[2],3])
    [1,2,3]

This mechanism is exactly what empowers Elixir references. Elixir references are uppercase identifiers (like `List`, `Orddict`, etc) that are converted to an atom representing a module at compilation time. For instance, by default `List` translates to the atom `::List`:

    iex> List
    ::List
    iex> is_atom(List)
    true

References are powerful when used with the `refer` directive discussed above. For instance, let's imagine our Math module relies heavily on the `Orddict` module. If, at some point, we find out most algorithms in `Orddict` could be implemented in a much faster way, we could implement `FastOrddict` and use it as a drop-in replacement:

    defmodule Math do
      refer FastOrddict, as: Orddict
      # ...
    end

Now any reference to `Orddict` will be automatically replaced by `FastOrddict`. In case one wants to access the original `Orddict`, it can be done by prefixing the module name with `::`:

    Orddict.values   #=> uses ::FastOrddict.values
    ::Orddict.values #=> uses ::Orddict.values

Finally, in Elixir `::` is simply an operator (like `+`). It is used to concatenate two references:

    iex> Foo::Bar
    ::Foo::Bar
    iex> Foo :: Bar
    ::Foo::Bar

[Chapter 2: Diving in](https://github.com/josevalim/elixir/blob/master/docs/2_diving_in.md) | [Index](https://github.com/josevalim/elixir/blob/master/docs/0_index.md) | [Chapter 4: Protocols & Records](https://github.com/josevalim/elixir/blob/master/docs/4_protocols_and_records.md)