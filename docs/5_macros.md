# 5 Macros

Elixir is an homoiconic language. Any Elixir program can be represented using its own data structures. This section describes the Elixir language specification for such data structures.

## 5.1 Building blocks of homoiconicity

The building block of Elixir homoiconicity is a tuple with three elements, for example:

    %{ :sum, 1, [1, 2, 3] }

The tuple above represents a function call to sum passing 1, 2 and 3 as arguments. The tuple elements are:

    %{ Tuple | Atom, Integer, List | Atom }

* The first element of the tuple is always an atom or another tuple in the same representation;
* The second element of the tuple is always an integer representing the line number;
* The third element of the tuple are the arguments for the function call. The third argument may also be an atom (`nil` or `:quoted`), meaning that the tuple may not represent a function call, but a variable.

You can get the representation of any expression by using the quote macro:

    iex> quote { sum(1, 2, 3) }
    %{ :sum, 0, [1, 2, 3] }

Everything in Elixir is a function call and can be represented by such tuples. For example, operators are represented as such:

    iex> quote { 1 + 2 }
    %{ :"+", 0, [1, 2] }

Even a tuple is represented as a call to `{}`:

    iex> quote { %{ 1, 2, 3 } }
    %{ :"%{}", 0, [1, 2, 3] }

The only exception to this rule are the five Elixir literals below. Literals are data types that when quoted return themselves. They are:

    :sum         #=> Atoms
    1.0          #=> Numbers
    [1,2]        #=> Lists
    "binaries"   #=> Binaries
    {key, value} #=> Key-value pairs (i.e. a tuple with two elements)

With those basic structures in mind, we are ready to define our own macro.

## 5.2 Defining our own macro

A macro can be define using `defmacro`. For instance, we can define a macro called `unless` which works the same as Ruby's unless in just few lines of code:

    defmodule MyMacro do
      defmacro unless(clause, options) do
        quote { if(!unquote(clause), unquote(options)) }
      end
    end

Similarly to `if`, `unless` expects two arguments: a `clause` and `options`:

    require MyMacro
    MyMacro.unless var, do: IO.puts "false"

However, since `unless` is a macro, it won't receive values when invoked, but instead, its expressions. For example, if one calls:

    unless 2 + 2 == 5, do: call_function()

Our `unless` macro will receive the following:

    unless(%{:==, 1, [%{:+, 1, [2, 2]}, 5]}, %{ :call_function, 1, [] })

Then our `unless` macro will call `quote`, to return a tree representation of the `if` clause. This means we are transforming our `unless` in a `if`!

However, there is a common mistake when quoting expressions which is that developers usually forget to `unquote` the proper expression. In order to understand what `unquote` does, let's simply remove it:

    defmacro unless(clause, options) do
      quote { if(!clause, options) }
    end

When called as `unless 2 + 2 == 5, do: call_function()`, our `unless` would then return:

    %{ :if, 0, [
      %{ :!, 0, [
        %{:custom, 0, quoted}
      ]},
      do: %{:options, 0, quoted}
    ] }

Notice that the tree structure returned by unless is trying to access `custom` and `options` as variables instead of using the `2 + 2 == 5` and `call_function()` expressions we passed as parameters. This is because we forgot to unquote. If we add `unquote` back:

    defmacro unless(clause, options) do
      quote { if(!unquote(clause), unquote(options)) }
    end

Which will then return:

    { :if, 0, [
      { :!, 0, [
        {:==, 1, [
          {:+, 1, [2, 2]},
          5
        ] }
      ] },
      do: { :call_function, 1, [] }
    ] }

In other words, unquote is a mechanism to inject expressions into the tree being quoted and is essential to the meta-programming mechanism. Elixir also provides `unquote_splicing` allowing us to inject many expressions at once.

We can define any macro we want, including override the built-in macros provided by Elixir. The only exceptions are Elixir special forms that cannot be overridden, [the full list of special forms is available in `Elixir::SpecialForms`](https://github.com/josevalim/elixir/tree/master/lib/elixir/special_forms.ex).

## 5.3 Macros hygiene

Elixir macros follow Scheme conventions and are hygienic. This means a variable defined inside a macro won't conflict with a variable defined in the context the macro is inserted. For example:

    defmodule Hygiene do
      defmacro no_interference do
        quote { a = 1 }
      end
    end

    require Hygiene

    a = 13
    Hygiene.no_interference
    a # => 13

In the example above, even if the macro injects `a = 1`, it does not affect the variable `a`. In case the macro wants to explicitly affect the context, it can use `var!`:

    defmodule Hygiene do
      defmacro interference do
        quote { var!(a) = 1 }
      end
    end

    require Hygiene

    a = 13
    Hygiene.interference
    a # => 1

Macros hygiene only works because Elixir marks a variable as coming from the quote. For example, consider this:

    iex> quote { x }
    %{ :x, 0, :quoted }

Notice that the third element is :quoted. It means that x may be a function call with 0 arguments or a variable coming from a quote. On the other hand, an unquoted variable would have the third element equals to nil. Let's consider this final example:

    defmodule Hygiene do
      defmacro quoted(x) do
        quote do
          %{ unquote(x), x, x() }
        end
      end
    end

In the example above, we have defined a macro called `quoted` that returns an unquoted variable, a quoted variable and a function call. Calling this macro will return:

    require Hygiene

    Hygiene.quoted(x)
    #=> {
      %{ :x, 1, nil },
      %{ :x, 1, :quoted },
      %{ :x, 1, [] }
    }

Summing up: if the third element is a list, it is certainly a function call. If not, it may be a variable or a function call. The variable is treated differently depending if it comes from a quote, or not.

## 5.4 Local functions and macros

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

With this note, we finish our introduction to macros. Next, let's move to the next chapter which will discuss several topics as native code compilation, partial application and others.

[Chapter 4: Protocols & Records](https://github.com/josevalim/elixir/blob/master/docs/4_protocols_and_records.md) | [Index](https://github.com/josevalim/elixir/blob/master/docs/0_index.md) |
[Chapter 6: Other topics](https://github.com/josevalim/elixir/blob/master/docs/6_other_topics.md)