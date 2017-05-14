# Writing Documentation

Elixir treats documentation as a first-class citizen. This means documentation should be easy to write and easy to read. In this document you will learn how to write documentation in Elixir, covering constructs like module attributes, style practices and doctests.

## Markdown

Elixir documentation is written using Markdown. There are plenty of guides on Markdown online, we recommend the ones available on GitHub as a getting started point:

  * [https://help.github.com/articles/markdown-basics/](https://help.github.com/articles/markdown-basics/)
  * [https://help.github.com/articles/markdown-basics/](https://help.github.com/articles/github-flavored-markdown/)

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

The `@moduledoc` attribute is used to add documentation to the module. `@doc` is used before a function to provide documentation for the function that follows. Besides the attributes above, `@typedoc` can also be used to attach documentation to types defined as part of typespecs.

## Function Arguments

When documenting a function, argument names are inferred by the compiler. For example:

    def size(%{size: size}) do
      size
    end

The compiler will infer this argument as `map`. Sometimes the inference will be suboptimal, especially if the function contains multiple clauses with the argument matching on different values each time. You can specify the proper names for documentation by declaring only the function head at any moment before the implementation:

    def size(map)
    def size(%{size: size}) do
      size
    end

## Recommendations

When writing documentation:

  * Keep the first paragraph of the documentation concise and simple, typically one line. Tools like [ExDoc](https://github.com/elixir-lang/ex_doc/) use the first line to generate a summary.

  * Reference modules by their full name.

Markdown uses backticks (`` ` ``) to quote code. Elixir builds on top of that to automatically generate links when module or function names are referenced. For this reason, always use full module names. If you have a module called `MyApp.Hello`, always reference it as `` `MyApp.Hello` `` and never as `` `Hello` ``.

  * Reference functions by name and arity if they are local, as in `` `world/1` ``, or by module, name and arity if pointing to an external module: `` `MyApp.Hello.world/1` ``.

  * Reference a `@callback` by prepending `c:`, as in `` `c:world/1` ``.

  * Reference a `@type` by prepending `t:`, as in `` `t:values/0` ``.

  * Start new sections with second level Markdown headers `##`. First level headers are reserved for module and function names.

  * Place documentation before the first clause of multi-clause functions. Documentation is always per function and arity and not per clause.

## Doctests

We recommend that developers include examples in their documentation, often under their own `## Examples` heading. To ensure examples do not get out of date, Elixir's test framework (ExUnit) provides a feature called doctests that allows developers to test the examples in their documentation. Doctests work by parsing out code samples starting with `iex>` from the documentation. You can read more about it in the documentation for `ExUnit.DocTest`.

Note that doctests have limitations. When you cannot doctest a function, because it relies on state or side-effects, we recommend developers include examples directly without the `iex>` prompt.

## Documentation is not Comments

Elixir treats documentation and code comments as different concepts. Documentation is for users of your Application Programming Interface (API), be it your co-worker or your future self. Modules and functions must always be documented if they are part of your API.

Code comments are for developers reading the code. They are useful to mark improvements, leave notes for developers reading the code (for example, you decided not to call a function due to a bug in a library), and so on.

In other words: documentation is required, code comments are optional.

## Hiding Internal Modules and Functions

Besides the modules and functions that libraries provide as part of their public interface, libraries may also implement important functionality that is not part of their API. While these modules and functions can be accessed, they are meant to be internal to the library and thus should not have documentation for end users.

Luckily, Elixir allows developers to hide modules and functions from the documentation. For example, one common practice for documenting internal behaviour is to set the `@moduledoc` attribute to `false` while documenting each function:

    defmodule MyApp.Hidden do
      @moduledoc false

      @doc """
      This function won't be listed in docs.
      """
      def function_that_wont_be_listed_in_docs do
        # ...
      end
    end

Similarly, developers can add `@doc false` to functions they do not want to show up in the documentation:

    defmodule MyApp.Sample do
      @doc false
      def add(a, b), do: a + b
    end

However, keep in mind that adding `@doc false` does not make the function private. The function above can still be invoked as `MyApp.Sample.add(1, 2)`. Not only that, if `MyApp.Sample` is imported, the `add/2` function will also be imported into the caller. For those reasons, be cautious when adding `@doc false` to functions, instead use one of these two options:

  * Move the undocumented function to a module with `@moduledoc false`, like `MyApp.Hidden`, ensuring the function won't be accidentally exposed or imported. Remember you can use `@moduledoc false` to hide a whole module and still document each function with `@doc`. Tools will still ignore the module.

  * Start the function name with one or two underscores, for example, `__add__/2`, and add `@doc false`. The compiler does not import functions with leading underscores and they suggest to anyone reading the code that they're meant to be used privately.

## Documenting Private Functions

Elixir warns if a private function has a `@doc` attribute and discards its content, because `@doc` is intended to be used only for your public interface.

Private functions may still need internal documentation for maintainers, though. That can be accomplished with code comments.

## Retrieving Documentation

Elixir stores documentation inside pre-defined chunks in the bytecode. It can be accessed from Elixir by using the `Code.get_docs/2` function. This also means documentation is only accessed when required and not when modules are loaded by the Virtual Machine. The only downside is that modules defined in-memory, like the ones defined in IEx, cannot have their documentation accessed as they do not have their bytecode written to disk.
