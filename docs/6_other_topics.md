# 6 Other topics

## 6.1 Documentation

Elixir uses the module data described in chapter 3 to drive its documentation system. For instance, consider the following example:

    defmodule MyModule do
      @moduledoc "It does X"

      @doc "Returns the version"
      def version, do: 1
    end

In the example above, we are adding a module documentation to MyModule via `@moduledoc` and using `@doc` to document each function. When compiled with the `--docs` option, we will be able to inspect the documentation attributes in runtime (remember to start iex in the same directory you compiled the module):

    $ elixirc my_module.ex --docs
    $ iex
    iex> MyModule.__info__(:docs)
    [{{:version,0},5,:def,"Returns the version"}]
    iex> MyModule.__info__(:moduledoc)
    {1,"It does X"}

`__info__(:docs)` returns a list of tuples where each tuple contains the function/arity pair, the line the function was defined, the kind of the function (`def` or `defmacro`, docs applied to `defp` are always ignored) and the comments. The comment should be either a binary or a boolean.

Similarly, `__info__(:moduledoc)` returns a tuple with the line the module was defined and its comments.

In case `--docs` is not provided during compilation, both calls would return nil. Elixir promotes the use of markdown in documentation, since it is a widely available format. Consider for example the documentation for `Module.add_doc` which allows us to dynamically add a documentation to a function:

    @doc """
    Attaches documentation to a given function. It expects
    the module the function belongs to, the line (a non negative
    integer), the kind (`:def` or `:defmacro`), a tuple representing
    the function and its arity and the documentation, which should
    be either a binary or a boolean.

    ## Examples

        defmodule MyModule do
          Module.add_doc(__MODULE__, __LINE__ + 1,
            :def, { :version, 0}, "Manually added docs")
          def version, do: 1
        end

    """
    def add_doc(module, line, kind, tuple, doc)

In the example, we use heredocs to allow the documentation to spawn several lines and markdown to style the documentation.

## 6.2 Partial application

Elixir also supports partial application. Let's suppose we have a list of strings and we want to calculate the size for each them. We could do it as follow:

    iex> list = ["foo", "bar", "baz"]
    ["foo","bar","baz"]
    iex> Enum.map list, fn(x) -> size(x) end
    [3,3,3]

However, with partial application, we could also do:

    iex> Enum.map list, size(&1)
    [3,3,3]

In the example above, we have invoked the function `size` passing `&1` as argument asking Elixir to generate a function that expects one argument and that argument will be passed to `size`.

Since operators are also function calls they can also be partially applied:

    iex> Enum.map [1,2,3], &1 * 2
    [2,4,6]

All functions can be partially applied, except [Elixir's special forms](https://github.com/josevalim/elixir/tree/master/lib/elixir/special_forms.ex).

## 6.3 Use

`use` is a macro intended to a common API for extension. For instance, in order to use the `ExUnit` test framework that ships with Elixir, you simply need to use `ExUnit::Case` in your module:

    defmodule AssertionTest do
      use ExUnit::Case

      def test_always_pass do
        true = true
      end
    end

By calling `use`, a hook called `__using__` will be invoked in `ExUnit::Case` which will then do the proper setup. In general, `use` is simply a translation to:

    defmodule AssertionTest do
      require ExUnit::Case
      ExUnit::Case.__using__(::AssertionTest)

      def test_always_pass do
        true = true
      end
    end

In general, we recommend APIs to expose a `__using__` hook in case they want to expose functionality to developers.

## 6.4 Comprehensions

Elixir also provides list and bit comprehensions. List comprehensions allow you to quickly build a list from another list:

    iex> lc n in [1,2,3,4], do: n * 2
    [2,4,6,8]

Or, using key-value blocks:

    lc n in [1,2,3,4] do
      n * 2
    end

A comprehension accepts several expressions. Those expressions can be generators, as in `x in [1,2,3,4]`, or filters:

    # A comprehension with a generator and a filter
    iex> lc n in [1,2,3,4,5,6], rem(n, 2) == 0, do: n
    [2,4,6]

    # A comprehension with two generators
    iex> lc x in [1,2], y in [2,3], do: x*y
    [2,3,4,6]

Elixir provides generators for both lists and bitstrings:

    # A list generator:
    iex> lc n in [1,2,3,4], do: n * 2
    [2,4,6,8]

    # A bit string generator:
    iex> lc <<n>> in <<1,2,3,4>>, do: n * 2
    [2,4,6,8]

Bit string generators are quite useful when you need to organize bit string streams:

    iex> pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>
    iex> lc <<r:8,g:8,b:8>> in pixels, do: {r,g,b}
    [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]

Remember, as strings are binaries and a binary is a bitstring, we can also use strings on comprehensions. For instance, the example below removes all white space characters from a string via bit comprehensions:

    iex> bc <<c>> in " hello world ", c != ?\s, do: <<c>>
    "helloworld"

Elixir does its best to hide the differences between list and bit string generators. However, there is a special case due to Erlang limitation that we need to explicitly tell Erlang that a list is being given as argument:

    # This will fail because when Elixir sees that the left side
    # of the in expression is a bit string, it expects the right side
    # to be a bit string as well:
    iex> lc <<n>> in [<<1>>,<<2>>,<<3>>], do: n*2
    ** (ErlangError) erlang error {:bad_generator,[<<1>>,<<2>>,<<3>>]}

    # You need to be explicit and use inlist:
    iex> lc inlist(<<n>>, [<<1>>,<<2>>,<<3>>]), do: n*2
    [2,4,6]

    # For consistency, inbin is also available:
    iex> lc inbin(<<n>>, <<1,2,3>>), do: n*2
    [2,4,6]

## 6.5 Native compilation

Elixir can compile to native code using the Hipe compiler. All you need to do is to export the following before running your code:

    export ERL_COMPILER_OPTIONS=native

[Chapter 5: Macros](https://github.com/josevalim/elixir/blob/master/docs/5_macros.md) | [Index](https://github.com/josevalim/elixir/blob/master/docs/0_index.md)
