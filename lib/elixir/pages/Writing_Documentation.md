# Writing Documentation

Elixir treats documentation as a first-class citizen. This means documentation should be easy to write and easy to read. In this document you will learn how to write documentation in Elixir, covering constructs like module attributes, style practices and doctests.

## Markdown

Elixir documentation is written using Markdown. There are plenty of guides on Markdown online, we recommend the ones available at GitHub as a getting started point:

  * https://help.github.com/articles/markdown-basics/
  * https://help.github.com/articles/github-flavored-markdown/

## Module Attributes

Documentation in Elixir is usually attached to module attributes. Let's see an example:

    defmodule MyApp.Hello do
      @moduledoc """
      This is the Hello module.
      """

      @doc """
      Says hello to the given `name`.

      Returns `:ok`.

      ## Examples

          iex> MyApp.Hello.world(:john)
          :ok

      """
      def world(name) do
        IO.puts "hello #{name}"
      end
    end

The `@moduledoc` attribute is used to add documentation to the module. `@doc` is used before a function to provide documentation for it. Besides the attributes above, `@typedoc` can also be used to attach documentation to types defined as part of typespecs.

## Function Arguments

When documenting a function, argument names are inferred by the compiler. For example:

    def size(%{size: size}) do
      size
    end

The compiler will infer this argument as `map`. Sometimes the inference will be suboptimal, specially if the function contains multiple clauses with the argument matching on different values each time. You can specify the proper names for documentation by using a bodyless clause:

    def size(map)

## Recommendations

There are a couple tips we recommend developers to follow when writing documentation:

  * Keep the first paragraph of the documentation concise and simple, typically one-line. Tools like ExDoc uses the first line to generate a summary.

  * Markdown uses backticks (`` ` ``) to quote code. Elixir builds on top of that to automatically generate links when modules or function names are referenced. For this reason, always use full module names. If you have a module called `MyApp.Hello`, always reference it as `` `MyApp.Hello` `` and never as `` `Hello` ``. Function names must be referenced by name and arity if they are local, as in `` `world/1` ``, or by module, name and arity if pointing to an external module: `` `MyApp.Hello.world/1` ``. Referencing a `@callback` can be done by prepending `c:`, as in `` `c:world/1` ``.

  * When the documentation has multiple sections, always start the section heading by using `##`. The first heading is reserved to the module or function name itself.

  * When documenting a function with multiple clauses, the documentation must be placed before the first clause. Documentation is always per function and not per clause.

## Doctests

We recommend developers to include examples in their documentation, often under its own `## Examples` heading. To ensure examples do not get out of date, Elixir's test framework (ExUnit) provides a feature called doctests that allows developers to test the examples in their documentation. Doctests work by parsing out code samples starting with `iex>` from the documentation. You can read more about it at `ExUnit.DocTest`.

Notice doctests have limitations. When you cannot doctest a function, because it relies on state or side-effects, we recommend developers to include examples directly without the `iex>` prompt.

## Documentation != Comments

Elixir treats documentation and code comments as different concepts. Documentation are for users of your Application Programming Interface (API), be it your co-worker or your future self. Modules and functions must always be documented if they are part of your API.

Code comments are for developers reading the code. They are useful to mark improvements, leave notes for developers reading the code (for example, you decided to not call a function due to a bug in a library) and so forth.

In other words, documentation is required, code comments are optional.

## Privacy

Elixir does not allow developers to document private functions. That's because private functions only exist inside the module that define them and cannot be accessed externally.

Luckily, Elixir allows developers to hide modules and functions from the documentation. For example, one common practice for documenting internal behaviour is to set the `@moduledoc` attribute to false while documenting each function:

    defmodule MyApp.Hidden do
      @moduledoc false

      @doc """
      This does something.
      """
      def function_that_wont_be_part_of_docs do
        # ...
      end
    end

Similarly, developers can add `@doc false` to functions they do not want to publicly expose:

    defmodule MyApp.Sample do
      @doc false
      def add(a, b), do: a + b
    end

However keep in mind adding `@doc false` does not make the function private. The function above can still be invoked as `MyApp.Sample.add(1, 2)`. Not only that, if the `MyApp.Sample` is imported, the `add/2` function will also be imported into the caller. For those reasons, be cautious when adding `@doc false` to functions, instead prefer one of:

  * Move the undocumented function to a module with `@moduledoc false`, like `MyApp.Hidden`, ensuring the function won't be accidentally exposed or imported. Remember you can use `@moduledoc false` to hide a whole module and still document each function with `@doc`. Tools will still ignore the module.

  * Start the function name with underscores, for example, `__add__/2`, and add `@doc false`. The compiler does not import functions with underscore and the underscore will hint of its intended private usage.

## Code.get_docs/2

Elixir stores documentation inside pre-defined chunks in the bytecode. It can be accessed from Elixir by using the `Code.get_docs/2` function. This also means documentation is only accessed when required and not when modules are loaded by the Virtual Machine. The only downside is that modules defined in-memory, like the ones defined in IEx, cannot have their documentation accessed as they do not have their bytecode written to disk.
